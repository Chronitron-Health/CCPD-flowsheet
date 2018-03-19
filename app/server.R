# add selection based on names



library(shiny)
library(shinythemes)
library(RSQLite)
library(tidyverse)
library(highcharter)

# which fields get saved 
fieldsAll <- c("name", "date", "weight", "temp", "bpsys", "bpdia", "bs", "perc", "meds", "dose", "indrain", "totUF",
               "avgDwell", "comments")

# which fields are mandatory
fieldsMandatory <- c("name", "weight", "temp", "bpsys", "bpdia", "bs", "perc", "indrain", "totUF", "avgDwell")

# add an asterisk to an input label
labelMandatory <- function(label) {
     tagList(
          label,
          span("*", class = "mandatory_star")
     )
}

# get current Epoch time
epochTime <- function() {
     return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
     format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
     con <- dbConnect(SQLite(), dbname="db/ccpdData.sqlite")
     # data$date <- as.Date(data$date)
     dbWriteTable(con, "test",as.data.frame(data),
                  append = TRUE)
     dbDisconnect(con)
}

# load all responses into a data.frame
loadData <- function() {
     con <- dbConnect(SQLite(), dbname="db/ccpdData.sqlite")
     data <- dbReadTable(con, "test")
     dbDisconnect(con)
     data <- data %>%
          select(-timestamp) %>%
          mutate(date = as.Date(date, origin = "1970-01-01"))
     data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
     ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# usernames that are admins
adminUsers <- c("admin", "akbar", "nurse")

# info for sharing this app on facebook/twitter
share <- list(
     title = "CCPD Flow Sheet",
     url = "https://akbaresfahani.shinyapps.io/ccpdFlowsheet/",
     image = "greenRibbon.jpg"
)


shinyServer(function(input, output, session) {
     #### UI code --------------------------------------------------------------
     output$ui <- renderUI({
          if (user_input$authenticated == FALSE) {
               ##### UI code for login page
               fluidPage(theme=shinytheme("slate"),
                    fluidRow(
                         column(width = 4, offset = 5,
                                br(), br(), br(), br(),
                                uiOutput("uiLogin"),
                                uiOutput("pass")
                         )
                    )
               )
          } else {
               #### Your app's UI code goes here!
               navbarPage(theme=shinytheme("united"),
                          title = "CCPD Flow Sheet",
                          tabPanel("Form",
                    shinyjs::useShinyjs(),
                    shinyjs::inlineCSS(appCSS),
                    
                    fluidRow(
                         column(6,
                                div(
                                     id = "form",
                                     
                                     textInput("name", labelMandatory("Name"), ""),
                                     dateInput("date", "Date:", Sys.Date()),
                                     textInput("weight", labelMandatory("Weight")),
                                     textInput("temp", labelMandatory("Temp")),
                                     textInput("bpsys", labelMandatory("BP-Systalic")),
                                     textInput("bpdia", labelMandatory("BP-Diastolic")),
                                     textInput("bs", labelMandatory("Blood Sugar")),
                                     selectInput("perc", labelMandatory("Solution Dose %"), choices = c("1.5 %", "2.5 %", "4.25 %")),
                                     textInput("meds", "Medication"),
                                     textInput("dose", "Dose"),
                                     textInput("indrain", labelMandatory("Initial Drain")),
                                     textInput("totUF", labelMandatory("Total UF")),
                                     textInput("avgDwell", labelMandatory("Average Dwell")),
                                     textInput("comments", "Comments"),
                                     actionButton("submit", "Submit", class = "btn-primary"),
                                     
                                     shinyjs::hidden(
                                          span(id = "submit_msg", "Submitting..."),
                                          div(id = "error",
                                              div(br(), tags$b("Error: "), span(id = "error_msg"))
                                          )
                                     )
                                ),
                                
                                shinyjs::hidden(
                                     div(
                                          id = "thankyou_msg",
                                          h3("Thanks, your response was submitted successfully!"),
                                          actionLink("submit_another", "Submit another response")
                                     )
                                )
                         ))
                    ),
                    tabPanel("Admin",
                             fluidRow(
                                  uiOutput("adminPanelContainer")
                             )
                    )
               )
          }
     })
     
     #### YOUR APP'S SERVER CODE GOES HERE ----------------------------------------
     # slider input widget
     observe({
          mandatoryFilled <-
               vapply(fieldsMandatory,
                      function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                      },
                      logical(1))
          mandatoryFilled <- all(mandatoryFilled)
          
          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
     })
     
     # Gather all the form inputs (and add timestamp)
     formData <- reactive({
          data <- sapply(fieldsAll, function(x) input[[x]])
          data <- c(data, timestamp = epochTime())
          data <- t(data)
          data
     })    
     
     # When the Submit button is clicked, submit the response
     observeEvent(input$submit, {
          
          # User-experience stuff
          shinyjs::disable("submit")
          shinyjs::show("submit_msg")
          shinyjs::hide("error")
          
          # Save the data (show an error message in case of error)
          tryCatch({
               saveData(formData())
               shinyjs::reset("form")
               shinyjs::hide("form")
               shinyjs::show("thankyou_msg")
          },
          error = function(err) {
               shinyjs::html("error_msg", err$message)
               shinyjs::show(id = "error", anim = TRUE, animType = "fade")
          },
          finally = {
               shinyjs::enable("submit")
               shinyjs::hide("submit_msg")
          })
     })
     
     # submit another response
     observeEvent(input$submit_another, {
          shinyjs::show("form")
          shinyjs::hide("thankyou_msg")
     })
     
     # render the admin panel
     output$adminPanelContainer <- renderUI({
          if (!isAdmin()) return()
          
          div(
               id = "adminPanel",
               h2("Previous responses (only visible to admins)"),
               downloadButton("downloadBtn", "Download responses"), br(), br(),
               
               highcharter::highchartOutput("vis1"),
               DT::dataTableOutput("responsesTable"))
     })
     
     
     # determine if current user is admin
     isAdmin <- reactive({
          input$user_name %in% adminUsers
     })    
     
     # Show the responses in the admin table
     output$responsesTable <- DT::renderDataTable(
          loadData(),
          rownames = FALSE,
          options = list(searching = FALSE, lengthChange = FALSE)
     )
     
     output$vis1 <- highcharter::renderHighchart({
          temp <- loadData()
          tempData <- temp %>% 
               select(name, date, bpsys, bpdia) %>%
               gather(., bp, values, -name, -date) %>% 
               arrange(date)
               hchart(tempData, "line", hcaes(x = date, y = values, group = bp),
                              color = c("#e5b13a", "#4bd5ee")) %>% 
               hc_title(text = "Blood Pressure history",
                        useHTML = TRUE) %>% 
               hc_tooltip(table = TRUE, sort = TRUE) %>% 
               hc_add_theme(
                    hc_theme_flatdark(
                         chart = list(
                              backgroundColor = "black"
                         )
                    )
               ) %>% 
               hc_xAxis(gridLineWidth = 0) %>% 
               hc_yAxis(gridLineWidth = 0)
     })
     
     # Allow user to download responses
     output$downloadBtn <- downloadHandler(
          filename = function() { 
               sprintf(paste0("patient ",input$ptname,"_%s.csv"), humanTime())
          },
          content = function(file) {
               write.csv(loadData(), file, row.names = FALSE)
          }
     )    

     
     #### PASSWORD server code ---------------------------------------------------- 
     # reactive value containing user's authentication status
     user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                                  user_locked_out = FALSE, status = "")
     
     # authenticate user by:
     #   1. checking whether their user name and password are in the credentials 
     #       data frame and on the same row (credentials are valid)
     #   2. if credentials are valid, retrieve their lockout status from the data frame
     #   3. if user has failed login too many times and is not currently locked out, 
     #       change locked out status to TRUE in credentials DF and save DF to file
     #   4. if user is not authenticated, determine whether the user name or the password 
     #       is bad (username precedent over pw) or he is locked out. set status value for
     #       error message code below
     observeEvent(input$login_button, {
          credentials <- readRDS("credentials/credentials.rds")
          
          row_username <- which(credentials$user == input$user_name)
          row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
          
          # if user name row and password name row are same, credentials are valid
          #   and retrieve locked out status
          if (length(row_username) == 1 && 
              length(row_password) >= 1 &&  # more than one user may have same pw
              (row_username %in% row_password)) {
               user_input$valid_credentials <- TRUE
               user_input$user_locked_out <- credentials$locked_out[row_username]
          }
          
          # if user is not currently locked out but has now failed login too many times:
          #   1. set current lockout status to TRUE
          #   2. if username is present in credentials DF, set locked out status in 
          #     credentials DF to TRUE and save DF
          if (input$login_button == num_fails_to_lockout & 
              user_input$user_locked_out == FALSE) {
               
               user_input$user_locked_out <- TRUE
               
               if (length(row_username) == 1) {
                    credentials$locked_out[row_username] <- TRUE
                    
                    saveRDS(credentials, "credentials/credentials.rds")
               }
          }
          
          # if a user has valid credentials and is not locked out, he is authenticated      
          if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
               user_input$authenticated <- TRUE
          } else {
               user_input$authenticated <- FALSE
          }
          
          # if user is not authenticated, set login status variable for error messages below
          if (user_input$authenticated == FALSE) {
               if (user_input$user_locked_out == TRUE) {
                    user_input$status <- "locked_out"  
               } else if (length(row_username) > 1) {
                    user_input$status <- "credentials_data_error"  
               } else if (input$user_name == "" || length(row_username) == 0) {
                    user_input$status <- "bad_user"
               } else if (input$password == "" || length(row_password) == 0) {
                    user_input$status <- "bad_password"
               }
          }
     })   
     
     # password entry UI componenets:
     #   username and password text fields, login button
     output$uiLogin <- renderUI({
          wellPanel(
               textInput("user_name", "User Name:"),
               
               passwordInput("password", "Password:"),
               
               actionButton("login_button", "Log in")
          )
     })
     
     # red error message if bad credentials
     output$pass <- renderUI({
          if (user_input$status == "locked_out") {
               h5(strong(paste0("Your account is locked because of too many\n",
                                "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
          } else if (user_input$status == "credentials_data_error") {    
               h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
          } else if (user_input$status == "bad_user") {
               h5(strong("User name not found!", style = "color:red"), align = "center")
          } else if (user_input$status == "bad_password") {
               h5(strong("Incorrect password!", style = "color:red"), align = "center")
          } else {
               ""
          }
     })  
})
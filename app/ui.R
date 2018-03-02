
library(shiny)
library(shinythemes)
# which fields get saved 
fieldsAll <- c("name", "date", "weight", "temp", "bpam", "bpm", "perc", "meds", "dose", "indrain", "totUF",
               "avgDwell", "comments")

# which fields are mandatory
fieldsMandatory <- c("name", "date", "weight", "temp", "bpam", "bppm", "perc", "indrain", "totUF", "avgDwell")

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
     fileName <- sprintf("%s_%s.csv",
                         humanTime(),
                         digest::digest(data))
     
     write.csv(x = data, file = file.path(responsesDir, fileName),
               row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
     files <- list.files(file.path(responsesDir), full.names = TRUE)
     data <- lapply(files, read.csv, stringsAsFactors = FALSE)
     #data <- dplyr::rbind_all(data)
     data <- do.call(rbind, data)
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
adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
     title = "CCPD Flow Sheet",
     url = "https://akbaresfahani.shinyapps.io/ccpdFlowsheet/",
     image = "greenRibbon.jpg"
)

shinyUI(
     uiOutput("ui")
)
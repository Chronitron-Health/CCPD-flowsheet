library(RSQLite)
con <- dbConnect(SQLite(), dbname="app/db/ccpdData.sqlite")
dbWriteTable(con, "test", temp, overwrite = T)
dbDisconnect(con)



#install.packages("RPostgres")
# app.R
## CDRA WITH LOGIN
#source("module/libraries.R", local = TRUE)
# Or
# Loading Libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyauthr)
library(shinymanager)
library(shinythemes)
library(htmltools)
library(fontawesome)
library(future)
library(shinyjs)
library(readxl)
library(xlsx)
library(rJava)
library(data.table)
library(leaflet)
library(leafgl)
library(sf)
library(knitr)
library(rmarkdown)
library(DBI)
library(dplyr)
library(glue)
library(scales)
library(ggplot2)
library(chron)
library(hms)
library(DT)
library(plotly)
library(data.table)
library(magrittr)
library(lubridate)
library(stringi)
library(webshot)
library(tinytex)
library(RODBC)
library(DT)
library(neo4r)
library(neo2R)

#library(shiny.telemetry)
#library(RMySQL)
#library(RPostgres)
#library(RPostgreSQL)
#library(mongolite)






# Settings & Options
options(encoding = "UTF-8")
options(shiny.maxRequestSize = 50 * 1024 ^ 2)
#Sys.setlocale(category = "LC_ALL", locale = "Arabic")
#addResourcePath("logo", "C:/Users/q6-47/Documents/Projects/Call-Detail-Records-Analysis/www")
#addResourcePath("logo", "/srv/shiny-server/cdra/www")

# Loading towers data and settings -> Server part
#source("module/towers_setting.R", local = TRUE)

# Loading criminal records ->  server part.
#source("module/matchingisisdatafunctions.R", local = TRUE)

# How many days should sessions last?
cookie_expiry <- 7

# This function must return a data.frame with columns user and session id.
# Other columns are also okay
# and will be made available to the app after log in.

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessions") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

# This function accept two parameters: user and session id. It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
  tibble(user = user,
         sessionid = sessionid,
         login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessions", ., append = TRUE)
  return(user)
}

# Here we connect to temp in memory db, creating table add credinsials to table !! Temporary !!
db <- dbConnect(RSQLite::SQLite(), ":memory:")

dbCreateTable(db,
              "sessions",
              c(
                user = "TEXT",
                sessionid = "TEXT",
                login_time = "TEXT"
              ))
user_base <- tibble(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"),
  password_hash = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)
# User Interface Module ?? maybe use log in first
ui <- shinyUI(fluidPage(source("parts/dashboardpage.R", local = TRUE)))

source("parts/server.R", local = TRUE)

shinyApp(ui = ui, server = server)
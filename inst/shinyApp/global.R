library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(readxl)
library(haven)
library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyexplorer)

# Set file upload size limit: 100 MB
options(shiny.maxRequestSize = 1024 * 1024 ^ 2)

ui     <- source("ui.R",     local = TRUE)
server <- source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)

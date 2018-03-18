library(shiny)
library(shinythemes)


load_data_sidebar <- function (id) {

  # Set namespace
  ns <- NS(id)

  # List of accepted file types for upload
  type_choices <- c(".csv", ".xls*")

  sidebarPanel(

    # Select filetype
    selectInput(
      ns("select_filetype"),
      label = "Select file type",
      choices = type_choices
    ),

    # Upload files
    uiOutput(ns("file_input_ui")),

    textOutput(ns("selected_filetype"))

  )

}

load_data_sidebar_server <- function (input, output, session) {

  ns <- session$ns

  output$file_input_ui <- renderUI({

    ns <- session$ns

    fileInput(
      ns("file_input"),
      label = "Upload files 2",
      multiple = TRUE
    )

  })

  output$selected_filetype <- renderText({
    "Here is some text"
  })

}


load_data_UI <- function (id) {

  # Set namespace
  ns <- NS(id)

  sidebarLayout(

    # Sidebar UI for load data page
    load_data_sidebar(ns("sidebar_panel")),

    # Main panel of load data page
    mainPanel("Here is some content")

  )

}


load_data_Server <- function (input, output, session) {

  # Get namespace
  ns <- session$ns

  callModule(load_data_sidebar_server, "sidebar_panel")

}


ui <- shinyUI(navbarPage(

  title = "Module test",

  tabPanel("Load data", load_data_UI("load_data"))

))


server <- function (input, output, session) {

  callModule(load_data_Server, "load_data")

}


shinyApp(ui = ui, server = server)

view_data_Server <- function (input, output, session, file_data) {

  ns <- session$ns

  output$select_table <- renderUI({
    selectInput(
      ns("select_table"),
      "Select dataset",
      choices = file_data()$name)
  })

  output$view_table <- DT::renderDataTable({

  })

}

view_data_UI <- function (id) {

  # Set namespace
  ns <- NS(id)

  sidebarLayout(

    # Sidebar UI for load data page
    sidebarPanel(
      width = 3,
      uiOutput(ns("select_table"))
      ),

    # Main panel of load data page
    mainPanel(
      # DT::dataTableOutput("")
      )

  )

}

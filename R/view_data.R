#' @export
#'

view_data_Server <- function (input, output, session, file_data) {

  ns <- session$ns

  output$select_table <- renderUI({
    req(file_data)

    selectInput(
      ns("select_table"),
      "Select dataset",
      choices = file_data()$name)

    })


  output$view_table <- DT::renderDataTable({

    req(file_data)

    df <- (file_data() %>%
      filter(name == input$select_table) %>%
      .$data)[[1]]

    colnames(df) <- df %>%
      colnames() %>%
      str_wrap(width = 30) %>%
      str_replace_all("\n", "<br>")

    dataTableThemed(df)

    })


  output$print_output <- renderPrint({
    df <- (file_data() %>%
             filter(name == input$select_table) %>%
             .$data)[[1]]

    colnames(df) <- df %>%
      colnames() %>%
      str_wrap(width = 30) %>%
      str_replace_all("\n", "<br>")

    cat(names(df), sep = "\n\n")
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
      DT::dataTableOutput(ns("view_table")),
      verbatimTextOutput(ns("print_output"))
      )

  )

}

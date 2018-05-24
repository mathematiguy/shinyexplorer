#' @export


load_data_sidebar <- function (id) {

  # Set namespace
  ns <- NS(id)

  # List of accepted file types for upload
  type_choices <- c(
    `Delimited file (.csv, .tsv)` = paste(
      "text/csv",
      "text/comma-separated-values",
      "text/plain",
      ".csv", ".tsv",
      sep = ","),
    `Excel file (.xls*)` = ".xls*",
    `SAS dataset (.sas7bdat)` = ".sas7bdat")

  type_condition <- function(type_name) {
    #' Create the condition for the conditionalPanel
    #' to match the selected file type
    sprintf(
      "input['%s-select_filetype'] == '%s'",
      id,
      type_choices[type_name])
  }

  sidebarPanel(

    # Select filetype
    selectInput(
      ns("select_filetype"),
      label = "Select file type:",
      choices = type_choices
    ),

    # Include panel for read_csv settings
    conditionalPanel(
      condition = type_condition('Delimited file (.csv, .tsv)'),
      settings_csv_input(ns("settings"))
    ),

    # Include panel for read_excel settings
    conditionalPanel(
      condition = type_condition('Excel file (.xls*)'),
      settings_excel_input(ns("settings"))
    ),

    # Upload files
    uiOutput(ns("file_input"))

  )

}


settings_csv_input <- function(id) {

  ns <- NS(id)

  tagList(

    fluidRow(

      column(
        width = 12,

        # Set delimiter
        radioButtons(
          inputId = ns("delim"),
          label = "Delimiter:",
          choices = c(`Comma` = ",", `Tab` = "\t", `Semicolon` = ";"),
          inline = TRUE)
      )
    ),

    fluidRow(

      column(
        width = 6,

        # Set skip rows
        numericInput(
          inputId = ns("skip"),
          label = "Rows to skip:",
          value = 0,
          width = '100%'),

        # Set header
        radioButtons(
          inputId = ns("header"),
          label = "Header row",
          choices = c(`Yes` = TRUE, `No` = FALSE),
          inline = TRUE)

      ),

      column(
        width = 6,

        # Set NA value
        textInput(
          inputId = ns("na"),
          label = "NA value:",
          value = "",
          width = "100%"
        )
      )

    )

  )

}


settings_excel_input <- function(id) {

  ns <- NS(id)

  tagList(

    fluidRow(

      column(
        width = 6,

        # Set skip rows
        numericInput(
          inputId = ns("skip"),
          label = "Rows to skip:",
          value = 0,
          width = '100%'),

        # Set Header
        radioButtons(
          inputId = ns("col_names"),
          label = "Header row",
          choices = c(`Yes` = TRUE, `No` = FALSE),
          inline = TRUE)

      ),

      column(
        width = 6,

        # Set NA value
        textInput(
          inputId = ns("na"),
          label = "NA value:",
          value = "",
          width = "100%"
        )
      )

    )

  )

}


load_data_sidebar_server <- function (input, output, session) {

  output$file_input <- renderUI({

    ns <- session$ns

    fileInput(
      ns("files_loaded"),
      label = "Upload files:",
      multiple = TRUE,
      accept = input$select_filetype
    )

  })

  format_filesize <- function(x) {
    #' converts the file size to a human readable character vector
    #' e.g. 1024 -> 1KB
    utils:::format.object_size(x, "auto")
  }

  res <- reactive({
    req(input$files_loaded)

    input$files_loaded %>%
      mutate(filetype = str_extract(name, "[^\\.]+$"),
             size = map_chr(size, format_filesize),
             data = map(datapath, read_csv))

    })

  return(res)

}


load_data_UI <- function (id) {

  # Set namespace
  ns <- NS(id)

  sidebarLayout(

    # Sidebar UI for load data page
    load_data_sidebar(ns("sidebar_panel")),

    # Main panel of load data page
    mainPanel(
      tabPanel(
        "Welcome",
        includeMarkdown("markdown/frontpage.md"),
        dataTableOutput(
          ns("file_loaded")),
        verbatimTextOutput(ns("file_data_print"))
      )
    )

  )

}


load_data_Server <- function (input, output, session) {

  load_data <- callModule(load_data_sidebar_server, "sidebar_panel")

  # file_data <- reactive({
  #
  #   req(load_data())
  #
  #
  #
  # })

  make_buttons <- function(n, id, type, text) {
    #' returns a character vector describing n buttons to be
    #' inserted into a dataTableOutput
    sprintf(
      '<button
          type="button"
          class="btn btn-%s"
          id="%s_%d">
       %s
       </button>', type, id, 1:n, text)
  }

  output$file_data_print <- renderPrint(load_data())

  output$file_loaded <- renderDataTable({

    req(load_data)

    data <- load_data() %>%
      mutate(actions = make_buttons(nrow(.),
               'delete', 'danger', "Delete")) %>%
      select(actions, name, filetype, size) %>%
      DT::datatable(
        style = "bootstrap",
        escape = F,
        rownames = FALSE,
        options = list(
          dom = 't',
          pageLength = 10,
          searching = FALSE,
          scrollX = TRUE,
          deferRender = TRUE,
          scroller = TRUE
        ))
    })

  return(load_data)
}

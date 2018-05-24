get_dataset <- function(datasets, input, ns) {
  
  sample_ns  <- paste0(ns, "-sample_size")
  dataset_ns <- paste0(ns, "-dataset")
  
  data <- datasets[[input[[dataset_ns]]]]
  sample_size <- input[[sample_ns]]
  
  if (sample_size == nrow(data)) {
    return(data)
  } else {
    return(sample_n(data, sample_size))
  }
  
}


select_dataset_ui <- function(id, label = "select_dataset") {

  ns <- NS(id)

  tagList(
    uiOutput(ns("dataset")),
    uiOutput(ns("sample_size"))
  )

}

select_dataset_server <- function(input, output, session) {

  ns <- session$ns

  data <- reactive({

    req(input$dataset)

    datasets[[input$dataset]]

  })

  output$sample_size <- renderUI({
    sliderInput(
      ns("sample_size"),
      label = "Sample size",
      min = 1,
      max = nrow(data()),
      value = min(10000, nrow(data()))
    )
  })

  output$dataset <- renderUI({
    selectInput(
      ns("dataset"),
      label = "Select dataset",
      choices = names(datasets)
    )
  })

  return(data)

}

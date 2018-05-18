library(shiny)
library(data.table)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider1", "Slider 1",min = 0.1, max = 1, value = 0.4, step = 0.05),
      sliderInput("slider2", "Slider 2",min = 0.1, max = 1, value = 0.4, step = 0.05),
      sliderInput("slider3", "Slider 3",min = 100, max = 20000, value = 5000, step= 200)
    ),
    mainPanel(
      tableOutput("tableOut")

    )

  ))

server <- function(input, output, session){

  #initalization - can this be put somewhere else?
  ret <- data.table(slider_names= c("slider1","slider2", "slider3"),
                    some_grouping_information=c("A", "A", "B") )


  rv_dtWeights <- reactive({
    ret[,original_values :=c(input$slider1,input$slider2,input$slider3)]
    ret[,working_values:=sum(original_values), by=some_grouping_information]
  })

  output$tableOut<- renderTable(rv_dtWeights())

}
shinyApp(ui = ui, server=server)

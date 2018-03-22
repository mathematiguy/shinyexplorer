#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  # outputOptions(output, "foo", suspendWhenHidden=FALSE)

  file_data <- callModule(load_data_Server, "load_data")

  callModule(view_data_Server, "view_data", file_data = file_data)

})

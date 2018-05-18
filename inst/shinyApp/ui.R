#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application
ui <- shinyUI(

  tagList(
    shinyjs::useShinyjs(),

    navbarPage(
      title = "Shiny Explorer",
      theme = shinytheme("cosmo"),
      fluid = TRUE,
      collapsible = TRUE,

      tabPanel(
        title = "Load data",
        load_data_UI("load_data")
      ),
      tabPanel(
        "View Data",
        view_data_UI("view_data")
      ),
      tabPanel(
        "Settings"
      )
  )
))

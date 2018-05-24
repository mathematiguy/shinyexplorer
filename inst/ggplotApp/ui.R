#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Define UI for application
ui <- shinyUI(navbarPage(

  title = "Shiny Explorer",
  theme = shinytheme("simplex"),
  fluid = TRUE,
  collapsible = TRUE,
  header = includeCSS("www/injury-theme.css"),

  tabPanel(
    shinyjs::useShinyjs(),
    title = "Scatter",
    sidebarLayout(

      sidebarPanel(

        actionButton(
          "scatter_update_button",
          "Update Plot",
          class = "update_button"
        ),
        tabsetPanel(
          tabPanel(
            "Settings",
            tags$br(),
            select_dataset_ui("scatter"),
            hr(),
            uiOutput("select_x"),
            uiOutput("select_y"),
            uiOutput("select_colour"),
            uiOutput("select_cat"),
            checkboxGroupButtons(
              inputId = "scatter_log_axes",
              label = "Logarithmic axes",
              choices = c("x axis", "y axis"),
              justified = TRUE,
              status = "primary",
              individual = TRUE,
              checkIcon = list(
                yes = icon("ok",     lib = "glyphicon"),
                no  = icon("remove", lib = "glyphicon"))
            ),
            sliderInput(
              "slider_alpha",
              label = "Opacity",
              min = 0,
              max = 1,
              value = 1)
          ),
          tabPanel(
            "Help",
            includeMarkdown("www/help-text/scatter.md")))

      ),

      mainPanel(
        plotOutput("scatter_plot", height = "800px")
      )
    )

  ),

  tabPanel(
    title = "Time Series",
    sidebarLayout(
      sidebarPanel(

        actionButton(
          "ts_update_button",
          "Update Plot",
          class = "update_button"),

        tabsetPanel(
          tabPanel(
            "Settings",
            tags$br(),
            select_dataset_ui("ts"),
            hr(),
            uiOutput("select_ts_x"),
            uiOutput("select_ts_colour"),
            uiOutput("select_ts_y"),
            selectInput(
              "ts_aggregate",
              label = "Aggregate by",
              choices = c("Year", "Six Month", "Month", "Quarter", "Day")
            ),
            selectInput(
              "ts_func",
              label = "Aggregation function",
              choices = names(agg_funcs)),
            uiOutput("date_select")
          ),
          tabPanel(
            "Help",
            includeMarkdown("www/help-text/time-series.md")))
      ),

      mainPanel(
        plotOutput("ts_plot", height = "800px")
        )
    )

  ),
  tabPanel(
    "Histograms",
    sidebarLayout(

      sidebarPanel(
        actionButton(
          "hist_update_button",
          "Update Plot",
          class = "update_button"
        ),

        tabsetPanel(
          tabPanel(
            "Settings",
            tags$br(),
            select_dataset_ui("hist"),
            hr(),
            uiOutput("hist_select_x"),
            uiOutput("select_hist_colour"),
            uiOutput("select_hist_cat"),
            radioGroupButtons(
              inputId = "hist_plot_type",
              label = "Plot type",
              choices = c("Histogram", "Density"),
              selected = "Histogram",
              justified = TRUE,
              status = "primary",
              checkIcon = list(
                yes = icon("ok",     lib = "glyphicon"),
                no  = icon("remove", lib = "glyphicon"))
            ),
            radioGroupButtons(
              inputId = "hist_plot_position",
              label = "Position",
              choices = c("Stack", "Dodge"),
              justified = TRUE,
              status = "primary",
              checkIcon = list(
                yes = icon("ok",     lib = "glyphicon"),
                no  = icon("remove", lib = "glyphicon"))
            ),
            checkboxGroupButtons(
              inputId = "hist_log_axes",
              label = "Logarithmic axes",
              choices = c("x axis", "y axis"),
              justified = TRUE,
              status = "primary",
              checkIcon = list(
                yes = icon("ok",     lib = "glyphicon"),
                no  = icon("remove", lib = "glyphicon"))
            ),
            uiOutput("hist_binwidth"),
            sliderInput(
              inputId = "hist_alpha",
              label = "Opacity",
              min = 0,
              max = 1,
              value = 1
            )
          ),
          tabPanel(
            "Help",
            includeMarkdown("www/help-text/histogram.md"))
          )
        ),


      mainPanel(
        plotOutput("hist_plot", height = "800px")
      )

    )

  ),
  tabPanel(
    "Bar Chart",
    sidebarLayout(

      sidebarPanel(
        actionButton(
          "bar_update_button",
          "Update Plot",
          class = "update_button"
        ),

        tabsetPanel(
          tabPanel(
            "Settings",
            tags$br(),
            select_dataset_ui("bar"),
            hr(),
            uiOutput("bar_select_x"),
            uiOutput("bar_select_y"),
            uiOutput("bar_select_colour"),
            uiOutput("bar_select_facet"),
            selectInput(
              "bar_func",
              label = "Aggregation function",
              choices = names(agg_funcs)),
            checkboxGroupButtons(
              inputId = "bar_buttons",
              label = "Controls",
              choices = c("Sort by value", "Flip axes"),
              justified = TRUE,
              status = "primary",
              checkIcon = list(
                yes = icon("ok",     lib = "glyphicon"),
                no  = icon("remove", lib = "glyphicon"))
            ),
            uiOutput("bar_slider")
          ),

          tabPanel(
            "Help",
            includeMarkdown("www/help-text/bar-chart.md"))
            )
        ),


        mainPanel(
          plotOutput("bar_plot", height = "800px")
        )

    )

  ),
  
  tabPanel(
    "Flow Chart",
    sidebarLayout(
      sidebarPanel(
        actionButton(
          "sankey_update_button",
          "Update Plot",
          class = "update_button"
        ),

        tabsetPanel(
          tabPanel(
            "Settings",
            tags$br(),
            select_dataset_ui("sankey"),
            hr(),
            uiOutput("sankey_select_numeric"),
            uiOutput("sankey_select_categorical_1"),
            uiOutput("sankey_select_categorical_2"),
            uiOutput("sankey_select_categorical_3"),
            uiOutput("sankey_select_categorical_4")
            ),
          tabPanel(
            "Help",
            includeMarkdown("www/help-text/flow-chart.md")
            )
          )

        ),
        mainPanel(
          sankeyNetworkOutput("sankey_plot", height = "800px")
        )
      )
    ),
    tabPanel(
      "Word Cloud",
      sidebarLayout(
        sidebarPanel(
          actionButton(
            "wc_update_button",
            "Update Plot",
            class = "update_button"),

          tabsetPanel(
            tabPanel(
              "Settings",
              tags$br(),
              select_dataset_ui("wordcloud"),
              hr(),
              uiOutput("wordcloud_var"),
              checkboxGroupButtons(
                inputId = "wc_controls_1",
                label = "Controls",
                choices = c("Lowercase text", "Remove numbers"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              checkboxGroupButtons(
                inputId = "wc_controls_2",
                label = "",
                choices = c("Remove punctuation", "Remove common english"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              checkboxGroupButtons(
                inputId = "wc_controls_3",
                label = "",
                choices = c("Remove plurals", "Stem tokens"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              sliderInput(
                "wc_ngram_slider",
                label = "ngram length",
                min = 1L,
                max = 6L,
                value = c(1L, 1L),
                step = 1
              ),
              fluidRow(
                column(
                  width = 6,
                  numericInput(
                    "wc_min_term_count",
                    label = "Min term count",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    "wc_min_token_length",
                    label = "Min token length",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              ),
              radioGroupButtons(
                "wc_scale_func",
                label = "Scale function",
                choices = c("Identity", "Log", "Sqrt"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              textAreaInput(
                "wc_stopwords",
                label = "Other words to remove",
                value = "",
                placeholder = "Insert words separated by commas"
              )
            ),
            tabPanel(
              "Help",
              includeMarkdown("www/help-text/word-cloud.md")
              )
            )
          ),

          mainPanel(
            wordcloud2Output("wordcloud_plot", height = "800px")
          )
      )

    ),
    tabPanel(
      "Word Clustering",
      sidebarLayout(
        sidebarPanel(
          actionButton(
            "cluster_update_button",
            "Update Plot",
            class = "update_button"),
          tabsetPanel(
            tabPanel(
              "Settings",
              tags$br(),
              select_dataset_ui("cluster"),
              hr(),
              uiOutput("cluster_select"),
              selectInput(
                "cluster_method",
                label = "String metric",
                choices = c(
                  "lv", "osa", "dl", "hamming", "lcs", "qgram",
                  "cosine", "jaccard", "jw", "soundex"),
                selected = "lv"),
              sliderInput(
                "cluster_text_size",
                label = "Text size",
                min = 0,
                max = 20,
                value = 5,
                step = 1
              )
            ),
            tabPanel(
              "Help",
              "Here is some help text!")
          )

        ),
        mainPanel(
          plotOutput("cluster_plot", height = "800px")
        )
      )
    ),
    tabPanel(
      "TF-IDF",
      sidebarLayout(
        sidebarPanel(
          actionButton(
            "tfidf_update_button",
            "Update Plot",
            class = "update_button"),
          tabsetPanel(
            tabPanel(
              "Settings",
              tags$br(),
              select_dataset_ui("tfidf"),
              hr(),
              sliderInput(
                "tfidf_numterms",
                label = "Number of terms",
                min = 1,
                max = 20,
                value = 5
              ),
              radioButtons(
                "tfidf_measure",
                label = "Measure",
                choiceNames = c("TF-IDF", "Term count"),
                choiceValues = c("tf_idf", "term_count"),
                inline = TRUE
              ),
              uiOutput("tfidf_var"),
              uiOutput("tfidf_select_cat"),
              checkboxGroupButtons(
                inputId = "tfidf_controls_1",
                label = "Controls",
                choices = c("Lowercase text", "Remove numbers"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              checkboxGroupButtons(
                inputId = "tfidf_controls_2",
                label = "",
                choices = c("Remove punctuation", "Use ngrams"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              checkboxGroupButtons(
                inputId = "tfidf_controls_3",
                label = "",
                choices = c("Remove plurals", "Stem tokens"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              checkboxGroupButtons(
                inputId = "tfidf_controls_4",
                label = "",
                choices = c("Remove common english"),
                justified = TRUE,
                status = "primary",
                checkIcon = list(
                  yes = icon("ok",     lib = "glyphicon"),
                  no  = icon("remove", lib = "glyphicon"))
              ),
              uiOutput("tfidf_ngram_slider"),
              fluidRow(
                column(
                  width = 6,
                  numericInput(
                    "tfidf_min_term_count",
                    label = "Min term count",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                ),
                column(
                  width = 6,
                  numericInput(
                    "tfidf_min_token_length",
                    label = "Min token length",
                    value = 1,
                    min = 1,
                    step = 1
                  )
                )
              ),
              textAreaInput(
                "tfidf_stopwords",
                label = "Stopwords",
                value = "",
                placeholder = "Insert words separated by commas"
              )
            ),
            tabPanel(
              "Help",
              "Here is some help text!")
            )

        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Bar Plot",
              plotOutput("tfidf_plot", height = "800px")
            ),
            tabPanel(
              "Word cloud",
              plotOutput("tfidf_wordclouds", height = "800px")
            )
          )
        )
      )
    ),
    tabPanel(
      "Settings",

      navlistPanel(
        tabPanel(
          "Global Settings",
          mainPanel(
            h2("Global settings"),
            tags$br(),
            fluidRow(
              column(
                width = 6,
                sliderInput(
                  "global_height",
                  label = "Plot height",
                  min = 400,
                  max = 1600,
                  value = 800,
                  step = 100
                ),
                sliderInput(
                  "global_width",
                  label = "Plot width",
                  min = 400,
                  max = 1600,
                  value = 800,
                  step = 100
                )
              ),
              column(
                width = 6,
                selectInput(
                  "global_ggplot_theme",
                  label = "Plot theme",
                  choices = gg_themes
                ),
                sliderInput(
                  "global_font_size",
                  label = "Font size",
                  min = 400,
                  max = 1600,
                  value = 800,
                  step = 100
                  )
                )
              )
            )
          ),
        tabPanel(
          "Scatter"
          ),
        tabPanel(
          "Time Series"
        ),
        tabPanel(
          "Histograms"
          ),
        tabPanel(
          "Bar Chart"
          ),
        tabPanel(
          "Flow Chart"
          ),
        tabPanel(
          "Word cloud"
        ),
        tabPanel(
          "Word clustering"
        ),
        tabPanel(
          "TF-IDF"
        )
      )
    )

))

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  callModule(select_dataset_server, "scatter")

  callModule(select_dataset_server, "ts")
  
  callModule(select_dataset_server, "hist")
  
  callModule(select_dataset_server, "bar")
  
  callModule(select_dataset_server, "sankey")
  
  callModule(select_dataset_server, "wordcloud")
  
  callModule(select_dataset_server, "cluster")
  
  callModule(select_dataset_server, "tfidf")
  
  output$select_x <- renderUI({
    
    req(input[["scatter-sample_size"]], input[["scatter-dataset"]])
    
    selectInput(
      "select_x",
      label = "Select x variable",
      choices = datasets %>%
        get_dataset(input, "scatter") %>% 
        select_if(is.numeric) %>%
        names()
    )
    
  })
  
  output$select_y <- renderUI({
    
    req(input[["scatter-sample_size"]], input[["scatter-dataset"]])
    
    selectInput(
      "select_y",
      label = "Select y variable",
      choices = datasets %>%
        get_dataset(input, "scatter") %>% 
        select_if(is.numeric) %>%
        names()
    )
    
  })
  
  output$select_colour <- renderUI({
    
    req(input[["scatter-sample_size"]], input[["scatter-dataset"]])
    
    selectInput(
      "select_colour",
      label = "Select colour variable",
      choices = c("None", datasets %>%
        get_dataset(input, "scatter") %>% 
        select_if(is.factor) %>%
        names())
    )
    
  })
  
  output$select_cat <- renderUI({
    
    req(input[["scatter-sample_size"]], input[["scatter-dataset"]])
    
    selectInput(
      "select_cat",
      label = "Select categorical variable",
      choices = c("None", datasets %>%
        get_dataset(input, "scatter") %>% 
        select_if(is.factor) %>%
        names())
    )
    
  })

  scatter_plot <- eventReactive(
    input$scatter_update_button, {
      req(input[["scatter-sample_size"]], input[["scatter-dataset"]])

      plot_colour <- if (input$select_colour == "None") NULL else input$select_colour

      plot <- datasets %>%
        get_dataset(input, "scatter") %>%
        ggplot(aes_string(x = input$select_x, y = input$select_y, colour = plot_colour)) +
        geom_point(alpha = input$slider_alpha) +
        theme_minimal()

      if (input$select_cat != "None") {
        plot = plot + facet_wrap(
          as.formula(paste0("~", input$select_cat))
        )
      }

      if ("x axis" %in% input$scatter_log_axes) {
        plot <- plot + scale_x_log10()
      }

      if ("y axis" %in% input$scatter_log_axes) {
        plot <- plot + scale_y_log10()
      }

      return(plot)

    },

    ignoreNULL = FALSE

  )

  output$scatter_plot <- renderPlot(scatter_plot())
  
  output$select_ts_x <- renderUI({
    
    req(input[["ts-sample_size"]], input[["ts-dataset"]])
    
    selectInput(
      "select_ts_x",
      label = "Select time variable",
      choices = datasets %>%
        get_dataset(input, "ts") %>% 
        select_if(is.Date) %>%
        names())
    
  })
  
  output$select_ts_colour <- renderUI({
    
    req(input[["ts-sample_size"]], input[["ts-dataset"]])
    
    selectInput(
      "select_ts_colour",
      label = "Select colour variable",
      choices = datasets %>%
        get_dataset(input, "ts") %>% 
        select_if(is.factor) %>%
        names())
    
  })
  
  output$select_ts_y <- renderUI({
    
    req(input[["ts-sample_size"]], input[["ts-dataset"]])
    
    selectInput(
      "select_ts_y",
      label = "Select y variable",
      choices = c("Count", datasets %>%
        get_dataset(input, "ts") %>% 
        select_if(is.numeric) %>%
        names()))
    
  })

  output$date_select <- renderUI({
    
    req(input[["ts-sample_size"]], input[["ts-dataset"]], input$select_ts_x)
    
    data <- datasets %>%
      get_dataset(input, "ts")
    
    date_min <- min( data[,input$select_ts_x][[1]], na.rm = TRUE)
    date_max <- max( data[,input$select_ts_x][[1]], na.rm = TRUE)

    sliderInput(
      "date_select",
      "Select date range",
      min   = date_min,
      max   = date_max,
      value = c(date_min, date_max),
      timeFormat = "%d/%m/%Y"
    )
    
  })

  ts_plot <- eventReactive(
    input$ts_update_button, {

      req(input[["ts-sample_size"]], input[["ts-dataset"]], input$date_select)

      plot_colour <- if (input$select_ts_colour == "None") NULL else input$select_ts_colour

      claim_data <- datasets %>%
        get_dataset(input, "ts") %>%
        filter_at(
          input$select_ts_x,
          .vars_predicate = all_vars(input$date_select[[1]] < . & . < input$date_select[[2]])) %>%
        mutate(Count = 1,
               y = year(   .[,input$select_ts_x][[1]]),
               m = month(  .[,input$select_ts_x][[1]]),
               d = day(    .[,input$select_ts_x][[1]]),
               q = quarter(.[,input$select_ts_x][[1]])) %>%
        mutate(Year        = ymd(paste0(y,"-01-01")),
               Month       = ymd(paste0(y, "-", m, "-01")),
               `Six Month` = ymd(paste0(y, "-", ((m - 1) %/% 6) * 6 + 1, "-01")),
               Quarter     = ymd(paste0(y, "-", q * 3 - 2, "-01")),
               Day         = ymd(paste0(y, "-", m, "-", d))) %>%
        group_by_at(c(plot_colour, input$ts_aggregate)) %>%
        summarise_at(input$select_ts_y, .funs = agg_funcs[input$ts_func][[1]]) %>%
        ungroup()

      claim_data %>%
        ggplot(aes_string(x = sprintf("`%s`", input$ts_aggregate),
                          y = input$select_ts_y, colour = plot_colour)) +
        geom_line() +
        scale_x_date() +
        theme_minimal()

    },

    ignoreNULL = FALSE

  )

  output$ts_plot <- renderPlot(ts_plot())

  output$hist_select_x <- renderUI({
    
    req(input[["hist-sample_size"]], input[["hist-dataset"]])
    
    selectInput(
      "hist_select_x",
      label = "Select numeric variable",
      choices = datasets %>%
        get_dataset(input, "hist") %>% 
        select_if(is.numeric) %>%
        names())
    
  })
  
  output$select_hist_colour <- renderUI({
    
    req(input[["hist-sample_size"]], input[["hist-dataset"]])
    
    selectInput(
      "select_hist_colour",
      label = "Select colour variable",
      choices = c("None", datasets %>%
        get_dataset(input, "hist") %>% 
        select_if(is.factor) %>%
        names()),
      selected = "None"
    )
    
  })
  
  output$select_hist_cat <- renderUI({
    
    req(input[["hist-sample_size"]], input[["hist-dataset"]])
    
    selectInput(
      "select_hist_cat",
      label = "Select categorical variable",
      choices = c("None", datasets %>%
          get_dataset(input, "hist") %>% 
          select_if(is.factor) %>%
          names()),
      selected = "None"
    )
    
  })
  
  hist_plot <- eventReactive(
    input$hist_update_button, {

      req(input[["hist-sample_size"]], input[["hist-dataset"]])

      plot_colour <- if (input$select_hist_colour == "None") NULL else input$select_hist_colour

      plot <- datasets %>%
        get_dataset(input, "hist") %>% 
        ggplot(aes_string(x = input$hist_select_x, fill = plot_colour)) +
        guides(alpha = FALSE) +
        theme_minimal()

      if ("Histogram" %in% input$hist_plot_type) {
        plot <- plot + geom_histogram(
          binwidth = input$hist_binwidth,
          alpha = input$hist_alpha,
          position = str_to_lower(input$hist_plot_position))
      }

      if ("Density" %in% input$hist_plot_type) {
        plot <- plot + geom_density(position = str_to_lower(input$hist_plot_position))
      }

      if ("x axis" %in% input$hist_log_axes) {
        plot <- plot + scale_x_log10()
      }

      if ("y axis" %in% input$hist_log_axes) {
        plot <- plot + scale_y_log10()
      }

      if (input$select_hist_cat != "None") {
        plot <- plot + facet_wrap(as.formula(paste0("~", input$select_hist_cat)))
      }

      return(plot)

    },

    ignoreNULL = FALSE

  )

  output$hist_plot <- renderPlot(hist_plot())

  output$bar_slider <- renderUI({
    data <- datasets %>%
      get_dataset(input, "bar") 
    
    bar_min <- 1
    bar_max <- length(unique(data[,input$bar_select_x][[1]]))

    sliderInput(
      "bar_slider",
      "Number of bars to display",
      min   = bar_min,
      max   = bar_max,
      value = bar_max,
      step = 1
    )

  })
  
  output$bar_select_x <- renderUI({
    
    req(input[["bar-sample_size"]], input[["bar-dataset"]])
    
    selectInput(
      "bar_select_x",
      label = "Select categorical variable",
      choices = datasets %>%
        get_dataset(input, "bar") %>%
        select_if(is.factor) %>%
        names())
    
  })
  
  output$bar_select_y <- renderUI({
    
    req(input[["bar-sample_size"]], input[["bar-dataset"]])
    
    selectInput(
      "bar_select_y",
      label = "Select numeric variable",
      choices = datasets %>%
        get_dataset(input, "bar") %>%
        select_if(is.numeric) %>%
        names())
    
  })
  
  output$bar_select_colour <- renderUI({
    
    req(input[["bar-sample_size"]], input[["bar-dataset"]])
    
    selectInput(
      "bar_select_colour",
      label = "Select colour variable",
      choices = c("None", datasets %>%
        get_dataset(input, "bar") %>%
        select_if(is.factor) %>%
        names()))
    
  })
  
  output$bar_select_facet <- renderUI({
    
    req(input[["bar-sample_size"]], input[["bar-dataset"]])
    
    selectInput(
      "bar_select_facet",
      label = "Select categorical variable",
      choices = c("None", datasets %>%
        get_dataset(input, "bar") %>%
        select_if(is.factor) %>%
        names()))
    
  })
  
  bar_plot <- eventReactive(
    input$bar_update_button, {

      req(input[["bar-sample_size"]], input[["bar-dataset"]])

      if ("Sort by value" %in% input$bar_buttons) {
        bar_select_x <- paste0("reorder(", input$bar_select_x, ", ", input$bar_select_y, ")")
      } else {
        bar_select_x <- input$bar_select_x
      }
      
      bar_select_colour <- if (input$bar_select_colour == "None") NULL else input$bar_select_colour
      
      group_vars <- input$bar_select_x
      
      if (input$bar_select_colour != "None") {
        group_vars <- c(input$bar_select_colour, group_vars)
      }
      
      if (input$bar_select_facet != "None") {
        group_vars <- c(input$bar_select_facet, group_vars)
      }

      plot_data <- datasets %>%
        get_dataset(input, "bar") %>%
        group_by_at(group_vars) %>%
        summarise_at(vars(input$bar_select_y), agg_funcs[input$bar_func][[1]])
      
      plot <- plot_data %>%
        head(input$bar_slider) %>%
        ggplot(aes_string(x = bar_select_x, y = input$bar_select_y, fill = bar_select_colour)) +
        geom_bar(stat = 'identity') +
        xlab(input$bar_select_x) +
        theme_minimal()
      
      if (input$bar_select_facet != "None") {
        plot <- plot + facet_wrap(as.formula(paste0("~", input$bar_select_facet)))
      }

      if ("Flip axes" %in% input$bar_buttons) {
        plot <- plot + coord_flip()
      }

      return(plot)

    },

    ignoreNULL = FALSE

  )

  output$bar_plot <- renderPlot(bar_plot())
  
  output$sankey_select_numeric <- renderUI({
    
    req(input[["sankey-sample_size"]], input[["sankey-dataset"]])
    
    selectInput(
      "sankey_select_numeric",
      label = "Select a numeric variable",
      choices = c("Count", datasets %>%
        get_dataset(input, "sankey") %>%
        select_if(is.numeric) %>%
        names())
    )
    
  })
  
  output$sankey_select_categorical_1 <- renderUI({
    
    req(input[["sankey-sample_size"]], input[["sankey-dataset"]])
    
    selectInput(
      "sankey_select_categorical_1",
      label = "Select a categorical variable",
      choices = datasets %>%
        get_dataset(input, "sankey") %>%
        select_if(is.factor) %>%
        names())
    
  })
  
  output$sankey_select_categorical_2 <- renderUI({
    
    req(input[["sankey-sample_size"]], input[["sankey-dataset"]])
    
    selectInput(
      "sankey_select_categorical_2",
      label = "Select a categorical variable",
      choices = (datasets %>%
        get_dataset(input, "sankey") %>%
        select_if(is.factor) %>%
        names()),
      selected = (datasets %>%
        get_dataset(input, "sankey") %>%
        select_if(is.factor) %>%
        names())[2])
    
  })
  
  output$sankey_select_categorical_3 <- renderUI({
    
    req(input[["sankey-sample_size"]], input[["sankey-dataset"]])
    
    selectInput(
      "sankey_select_categorical_3",
      label = "Select a categorical variable",
      choices = c("None", datasets %>%
        get_dataset(input, "sankey") %>%
        select_if(is.factor) %>%
        names()))
    
  })
  
  output$sankey_select_categorical_4 <- renderUI({
    
    req(input[["sankey-sample_size"]], input[["sankey-dataset"]])
    
    selectInput(
      "sankey_select_categorical_4",
      label = "Select a categorical variable",
      choices = c("None", datasets %>%
                    get_dataset(input, "sankey") %>%
                    select_if(is.factor) %>%
                    names()))
    
  })
  
  sankey_plot <- eventReactive(
    input$sankey_update_button, {

      req(input[["sankey-sample_size"]], input[["sankey-dataset"]])

      group_vars <- c(input$sankey_select_categorical_1,
                      input$sankey_select_categorical_2)

      if (input$sankey_select_categorical_3 != "None") {
        group_vars <- c(group_vars, input$sankey_select_categorical_3)
      }
      
      if (input$sankey_select_categorical_4 != "None") {
        group_vars <- c(group_vars, input$sankey_select_categorical_4)
      }

      claim_data <- datasets %>%
        get_dataset(input, "sankey") %>%
        mutate(Count = 1)

      sankey_data <- claim_data %>%
        group_by_at(vars(group_vars)) %>%
        summarise_at(
          input$sankey_select_numeric,
          .funs = sum) %>%
        build_sankey_data()

      sankeyNetwork(
        Links = sankey_data$links,
        Nodes = sankey_data$nodes,
        Source = 'source_id',
        Target = 'target_id',
        Value = 'value',
        NodeID = 'name',
        colourScale = sankey_data$colour_scale,
        fontSize = 14,
        fontFamily = "Open Sans",
        LinkGroup = 'target',
        units = 'cases')

    },

    ignoreNULL = FALSE

  )

  output$sankey_plot <- renderSankeyNetwork(sankey_plot())
  
  output$wordcloud_var <- renderUI({
    
    selectInput(
      "wordcloud_var",
      label = "Select character variable",
      choices = datasets %>%
        get_dataset(input, "wordcloud") %>% 
        select_if(is.character) %>%
        type_convert() %>%
        select_if(function(x) {
          return(is.character(x) & length(unique(x)) > 100)}) %>%
        names()
    )
    
  })

  wordcloud_plot <- eventReactive(
    input$wc_update_button, {

      req(input[["wordcloud-sample_size"]], input[["wordcloud-dataset"]])

      scale_funcs = c(Log = log, Sqrt = sqrt, Identity = function(x) x)

      claim_data <- datasets %>%
        get_dataset(input, "wordcloud")

      if (str_length(input$wc_stopwords) > 0) {
        stop_words <- str_split(
          input$wc_stopwords[[1]],
          pattern = ",\\s*")[[1]]
      } else {
        stop_words <- character(0)
      }

      claim_data[, input$wordcloud_var][[1]] %>%
        unique() %>%
        col_to_wordcloud(
          lowercase          = "Lowercase text"        %in% input$wc_controls_1,
          remove_punctuation = "Remove punctuation"    %in% input$wc_controls_2,
          deplural           = "Remove plurals"        %in% input$wc_controls_3,
          remove_numbers     = "Remove numbers"        %in% input$wc_controls_1,
          use_ngrams         = TRUE,
          ngram              = input$wc_ngram_slider,
          stem_document      = "Stem tokens"           %in% input$wc_controls_3,
          use_en_stopwords   = "Remove common english" %in% input$wc_controls_2,
          stop_words         = stop_words,
          min_term_count     = input$wc_min_term_count,
          min_token_length   = input$wc_min_token_length,
          scale_func         = input$wc_scale_func,
          fontFamily = "Helvetica"
        )
    },

    ignoreNULL = FALSE

  )

  output$wordcloud_plot <- renderWordcloud2(wordcloud_plot())
  
  output$cluster_select <- renderUI({
    
    req(input[["cluster-sample_size"]], input[["cluster-dataset"]])
    
    selectInput(
      "cluster_select",
      label = "Select variable",
      choices = datasets %>%
        get_dataset(input, "cluster") %>% 
        type_convert() %>%
        select_if(function (x) {
          num_uniques <- length(unique(x))
          (is.factor(x) | is.character(x)) & num_uniques > 1 & num_uniques <= 100}) %>%
        names()
    )
    
  })

  cluster_plot <- eventReactive(
    input$cluster_update_button, {
      
      req(input[["cluster-sample_size"]], input[["cluster-dataset"]])
      
      data <- get_dataset(datasets, input, "cluster")

      cluster_text <- unique(data[,input$cluster_select][[1]])
      cluster_text <- cluster_text[!is.na(cluster_text)]

      cluster_text %>%
        stringdistmatrix(., method = input$cluster_method) %>%
        prcomp() %>%
        .$x %>%
        as_tibble() %>%
        mutate(colname = cluster_text) %>%
        ggplot(aes(x = PC1, y = PC2, label = colname)) +
        geom_point() +
        geom_text_repel(size = input$cluster_text_size) +
        theme_minimal()
    },

    ignoreNULL = FALSE

  )

  output$cluster_plot <- renderPlot(cluster_plot())

  output$tfidf_var <- renderUI({
    
    req(input[["tfidf-sample_size"]], input[["tfidf-dataset"]])
    
    selectInput(
      "tfidf_var",
      label = "Select character variable",
      choices = datasets %>% 
        get_dataset(input, "tfidf") %>% 
        select_if(is.character) %>%
        type_convert() %>%
        select_if(function(x) {
          return(length(unique(x)) > 100)}) %>%
        names()
    )
    
  })
  
  output$tfidf_select_cat <- renderUI({
    
    req(input[["tfidf-sample_size"]], input[["tfidf-dataset"]])
    
    selectInput(
      "tfidf_select_cat",
      label = "Select categorical variable",
      choices = datasets %>% 
        get_dataset(input, "tfidf") %>% 
        select_if(is.factor) %>%
        names()
    )
    
  })
  
  output$tfidf_ngram_slider <- renderUI({

    if ("Use ngrams" %in% input$tfidf_controls_2) {
      sliderInput(
        "tfidf_ngram_slider",
        label = "ngram length",
        min = 1L,
        max = 6L,
        value = c(1L, 6L),
        step = 1
      )
    }

  })

  tfidf_plot <- eventReactive(
    input$tfidf_update_button, {

      req(input[["tfidf-sample_size"]], input[["tfidf-dataset"]])

      if (str_length(input$tfidf_stopwords) > 0) {
        stop_words <- str_split(
          input$tfidf_stopwords[[1]],
          pattern = ",\\s*")[[1]]
      } else {
        stop_words <- character(0)
      }

      datasets %>%
        get_dataset(input, "tfidf") %>%
        plot_tfidf(
          char_colname       = input$tfidf_var,
          cat_colname        = input$tfidf_select_cat,
          measure            = input$tfidf_measure,
          num_terms          = input$tfidf_numterms,
          lowercase          = "Lowercase text"        %in% input$tfidf_controls_1,
          remove_punctuation = "Remove punctuation"    %in% input$tfidf_controls_2,
          deplural           = "Remove plurals"        %in% input$tfidf_controls_3,
          remove_numbers     = "Remove numbers"        %in% input$tfidf_controls_1,
          use_ngrams         = "Use ngrams"            %in% input$tfidf_controls_2,
          ngram              = input$tfidf_ngram_slider,
          stem_document      = "Stem tokens"           %in% input$tfidf_controls_3,
          use_en_stopwords   = "Remove common english" %in% input$tfidf_controls_4,
          stop_words         = stop_words,
          min_term_count     = input$tfidf_min_term_count,
          min_token_length   = input$tfidf_min_token_length
        )

    },

    ignoreNULL = FALSE

  )

  output$tfidf_plot <- renderPlot(tfidf_plot())

  tfidf_wordclouds <- eventReactive(

    input$tfidf_update_button, {

      if (str_length(input$tfidf_stopwords) > 0) {
        stop_words <- str_split(
          input$tfidf_stopwords[[1]],
          pattern = ",\\s*")[[1]]
      } else {
        stop_words <- character(0)
      }

      datasets %>%
        get_dataset(input, "tfidf") %>%
        build_tfidf_data(
          char_colname       = input$tfidf_var,
          cat_colname        = input$tfidf_select_cat,
          measure            = input$tfidf_measure,
          num_terms          = input$tfidf_numterms,
          lowercase          = input$tfidf_lowercase,
          remove_punctuation = input$tfidf_remove_puncs,
          deplural           = input$tfidf_remove_plurals,
          remove_numbers     = input$tfidf_remove_numbers,
          use_ngrams         = input$tfidf_use_ngrams,
          ngram              = input$tfidf_ngram_slider,
          stem_document      = input$tfidf_stem_tokens,
          use_en_stopwords   = input$tfidf_use_stopwords,
          stop_words         = stop_words,
          min_term_count     = input$tfidf_min_term_count,
          min_token_length   = input$tfidf_min_token_length) %>%
        mutate(df  = tanh(1 / idf),
               col = log10(term_count) + 1) %>%
        ggplot(aes(x = 1, y = 1, size = df, label = term,
                   colour = category, alpha = col)) +
        geom_text_repel(segment.size = 0, force = 10) +
        guides(colour = FALSE, alpha = FALSE) +
        scale_size(range = c(2, 15), guide = FALSE) +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        facet_wrap(~category, scales = 'free') +
        labs(x = '', y = '') +
        theme_minimal()

    },

    ignoreNULL = FALSE

  )

  output$tfidf_wordclouds <- renderPlot(tfidf_wordclouds())

})


























































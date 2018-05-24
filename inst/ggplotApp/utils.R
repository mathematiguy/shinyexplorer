library(stringr)
library(text2vec)
library(tm)
library(wordcloud2)

build_vocab <- function(col, lowercase = TRUE, remove_punctuation = TRUE, deplural = TRUE,
                        remove_numbers = TRUE, use_ngrams = TRUE, ngram = c(1L, 2L),
                        stem_document = TRUE, use_en_stopwords = TRUE,
                        stop_words = character(0), min_term_count = 50) {

  cleanPunctuation <- function (s) {
    str_replace_all(s, "[:punct:]", "")
  }

  depluralise <- function(s) {
    str_replace_all(s, "s\\s+|s$", " ")
  }

  clean_text <- function (s) {

    if (lowercase) {
      s <- tolower(s)
    }

    if (remove_punctuation) {
      s <- cleanPunctuation(s)
    }

    if (deplural) {
      s <- depluralise(s)
    }

    if (remove_numbers) {
      s <- removeNumbers(s)
    }

    if (stem_document) {
      s <- stemDocument(s)
    }

    s <- stripWhitespace(s)

    return(s)

  }

  if (use_en_stopwords) {
    stop_words = c(stop_words, stopwords("en"))
  } else {
    stop_words = character(0)
  }

  if (!use_ngrams) {
    ngram = c(1L, 1L)
  }

  it = itoken(col,
              preprocessor = clean_text,
              tokenizer = word_tokenizer,
              id = nrow(length(col)),
              progressbar = FALSE)

  vocab = create_vocabulary(
    it,
    ngram = ngram,
    stopwords = stop_words)

  vocab = prune_vocabulary(vocab, term_count_min = min_term_count)

  return(vocab)

}

col_to_wordcloud <- function(col, lowercase = TRUE, remove_punctuation = TRUE, deplural = TRUE,
                             remove_numbers = TRUE, use_ngrams = TRUE, ngram = c(1L, 2L),
                             stem_document = TRUE, use_en_stopwords = TRUE,
                             stop_words = character(0), min_term_count = 50,
                             min_token_length = 1, scale_func = 'Identity',
                             fontFamily = "Helvetica") {

  scale_funcs = c(Log = log, Sqrt = sqrt, Identity = function(x) x)

  vocab <- build_vocab(
    col,
    lowercase = lowercase, remove_punctuation = remove_punctuation,
    deplural = deplural, remove_numbers = remove_numbers,
    use_ngrams = use_ngrams, ngram = ngram, stem_document = stem_document,
    use_en_stopwords = use_en_stopwords, stop_words = stop_words,
    min_term_count = min_term_count
  )

  vocab %>%
    as_tibble() %>%
    select(word = term, freq = term_count) %>%
    filter(str_length(word) > min_token_length) %>%
    mutate(freq = scale_funcs[scale_func][[1]](freq)) %>%
    arrange(desc(freq)) %>%
    wordcloud2(fontFamily = fontFamily)

}

build_tfidf_data <- function(data, char_colname, cat_colname, measure, num_terms = 5,
                             lowercase = TRUE, remove_punctuation = TRUE, deplural = TRUE,
                             remove_numbers = TRUE, use_ngrams = TRUE, ngram = c(1L, 2L),
                             stem_document = TRUE, use_en_stopwords = TRUE,
                             stop_words = character(0), min_term_count = 50,
                             min_token_length = 1) {

  tfidf_data <- data %>%
    group_by_at(vars(cat_colname)) %>%
    summarise(vocab = list(build_vocab(
      col = !!sym(char_colname),
      lowercase = lowercase,
      remove_punctuation = remove_punctuation,
      deplural = deplural,
      remove_numbers = remove_numbers,
      use_ngrams = use_ngrams,
      ngram = ngram,
      stem_document = stem_document,
      use_en_stopwords = use_en_stopwords,
      stop_words = stop_words,
      min_term_count = min_term_count))) %>%
    mutate(vocab = map2(vocab, !!sym(cat_colname),
                        function(v, o) mutate(v, category = o))) %>%
    .$vocab %>%
    bind_rows() %>%
    as_tibble()

  plot_data <- tfidf_data %>%
    bind_tf_idf(term, category, term_count) %>%
    filter(term != "NA", str_length(term) >= min_token_length) %>%
    arrange_at(vars("category", measure)) %>%
    group_by(category) %>%
    do(tail(., num_terms)) %>%
    ungroup() %>%
    mutate(order = row_number(),
           term2 = reorder(term, !!sym(measure)))

  return(plot_data)

}

plot_tfidf <- function(data, char_colname, cat_colname, measure, num_terms = 5,
                       lowercase = TRUE, remove_punctuation = TRUE, deplural = TRUE,
                       remove_numbers = TRUE, use_ngrams = TRUE, ngram = c(1L, 2L),
                       stem_document = TRUE, use_en_stopwords = TRUE,
                       stop_words = character(0), min_term_count = 50,
                       min_token_length = 1) {

  plot_data <- build_tfidf_data(
    data, char_colname, cat_colname, measure, num_terms = num_terms,
    lowercase = lowercase, remove_punctuation = remove_punctuation,
    deplural = deplural, remove_numbers = remove_numbers,
    use_ngrams = use_ngrams, ngram = ngram, stem_document = stem_document,
    use_en_stopwords = use_en_stopwords, stop_words = stop_words,
    min_term_count = min_term_count, min_token_length = min_token_length)

  plot_data %>%
    ggplot(aes_string(x = 'order', y = measure, fill = 'category')) +
    geom_bar(stat = 'identity') +
    facet_wrap(~category, scales = 'free') +
    scale_x_continuous(
      breaks = plot_data$order,
      labels = plot_data$term,
      expand = c(0, 0)) +
    guides(fill = FALSE) +
    coord_flip() +
    theme_minimal()

}

library(networkD3)

build_sankey_data <- function(data, colour_data = NULL) {
  #' This is a generic function for preparing datasets to be input into
  #' the `networkD3::sankeyNetwork` function.
  #'
  #' The columns are assumed to be in the form of:
  #' var_1 -> var_2 -> ... -> var_n -> value
  #' where var_i is the variable for the ith layer in the sankey chart.
  #'
  #' Assuming this order, `build_sankey_data` returns the Nodes and Links
  #' `data.frame`s for constructing the sankey chart correctly.

  # There is currently a bug in networkD3::sankeyNetwork which assigns
  # edge and vertex colours incorrectly if the regular space character
  # is used, so we replace it with a special character.
  space_char <- ' '

  data <- data %>%
    mutate_if(is.character, funs(str_replace_all(., " ", space_char)))

  # Get the variable name columns
  # Assumes that the values are stored in the last column
  gather_cols <- names(data)[1:(ncol(data) - 1)]
  value_col <- names(data)[ncol(data)]

  # Prepare the `node_data` by gathering `variable`/`name` combinations.
  # This is important because sometimes names are duplicated between
  # layers and we still need the ids to be distinct.
  #
  # Of course, `id`s are attached here too so we can inspect the table
  # later to make sure that the concordance is correct.
  node_data <- data %>%
    select(one_of(gather_cols)) %>%
    gather(one_of(gather_cols), key = 'variable', value = 'name') %>%
    distinct() %>%
    mutate(id = as.numeric(factor(paste(variable, name, sep = "//"))) - 1) %>%
    arrange(id)

  # Prepare `link_data` - first we create a list of dataframes of the
  # form source -> target -> value. These are later concatenated to form
  # the `link_data` for `networkD3::sankeyNetwork`
  link_list <- lapply(1:(ncol(data) - 2), function(i) {
    # The columns of `data` are split into n - 2 dataframes here.
    # The matching looks like:
    # names(df1) = c('var_1', 'var_2', 'value')
    # names(df2) = c('var_2', 'var_3', 'value')
    # ...
    # names(df[n-2]) = c('var_[n-2]', 'var_[n-1]', 'value')
    #
    # Before outputting, the columns are renamed to 'source', 'target'
    # and 'value'. This way the tables can be concatenated with `bind_rows`.

    # Extract the column names for this loop
    link_cols   <- names(data)[c(i, i + 1, ncol(data))]
    source <- link_cols[1]
    target <- link_cols[2]
    value  <- link_cols[3]

    # Select the columns
    res <- data[, link_cols]

    # Rename the columns
    names(res) <- c('source', 'target', 'value')

    # Aggregate the `value`s by `source` and `target`.
    # Then attach the `variable` names for each `source` and `target`.
    res <- res %>%
      group_by(source, target) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(source_var = link_cols[1],
             target_var = link_cols[2])

    return(res)

  })

  # Bind the tables in `link_list` together, then join with `node_data` to get
  # the correct `id`s for each link. Then, reorder the columns nicely and convert
  # the `id`s to integers and `link_data` to `data.frame` to pass the type checking
  # in `networkD3::sankeyNetwork`.
  link_data <- link_list %>%
    bind_rows() %>%
    inner_join(node_data, by = c("source_var" = "variable", "source" = "name")) %>%
    rename(source_id = id) %>%
    inner_join(node_data, by = c("target_var" = "variable", "target" = "name")) %>%
    rename(target_id = id) %>%
    select(source_var, target_var, source, target, source_id, target_id, value) %>%
    mutate(source_id = as.integer(source_id),
           target_id = as.integer(target_id)) %>%
    as.data.frame()

  # `node_data` must be a data.frame and the `name` column must be a factor.
  node_data <- node_data %>%
    mutate(name = factor(name)) %>%
    as.data.frame()

  build_d3_palette <- function(names, colours) {
    #' convert a character vector of hex colours into a d3 scaleOrdinal palette
    if (length(names) != length(colours)) {
      stop("names and colours lengths do not match")
    }
    name_list    <- jsonlite::toJSON(names)
    palette_list <- jsonlite::toJSON(colours)
    palette_text <- paste0('d3.scaleOrdinal()',
                           '.domain(', name_list,   ')',
                           '.range(', palette_list, ");")
    return(palette_text)
  }

  if (!is.null(colour_data)) {

    colour_data <- colour_data %>%
      mutate_if(is.character, funs(str_replace_all(., " ", space_char)))

    node_data <- node_data %>%
      left_join(colour_data, by = c('name'))

    node_colours <- node_data %>%
      select(name, colour) %>%
      distinct()

    colour_scale <- build_d3_palette(node_colours$name, node_colours$colour)

  } else {
    colour_scale <- "d3.scaleOrdinal(d3.schemeCategory20);"
  }

  # package `node_data` and `link_data` together before return
  res <- list(nodes = node_data, links = link_data, colour_scale = JS(colour_scale))

  return(res)

}









































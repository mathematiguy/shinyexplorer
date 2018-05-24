# look up: https://shiny.rstudio.com/articles/dynamic-ui.html
library(dplyr)
library(tidyr)
library(data.table)
library(magrittr)
library(readr)
library(purrr)
library(rlang)
library(stringr)
library(text2vec)
library(tm)
library(wordcloud2)
library(stringdist)
library(tidytext)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(ggplot2)
library(ggrepel)
library(networkD3)

set.seed(1234)

# Set file upload size limit: 100 MB
options(shiny.maxRequestSize = 1024 * 1024 ^ 2)

agg_funcs <- c(Sum = sum, Mean = mean, Median = median,
               Min = min, Max = max)

gg_themes <- c("default", "grey", "gray", "bw", "linedraw",
  "light", "dark", "minimal", "classic", "void", "test",
  "basetheme_", "calc", "economist", "excel", "few",
  "fivethirtyeight", "gdocs", "hc", "par", "pander",
  "solarized", "stata", "tufte", "wsj")

source("modules.R", local = TRUE)
source("utils.R", local = TRUE)
source("load_data.R", local = TRUE)

ui     <- source("ui.R",     local = TRUE)
server <- source("server.R", local = TRUE)

shinyApp(ui = ui, server = server)

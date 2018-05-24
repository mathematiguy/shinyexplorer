fix_dates <- function(x) {
  floor(x / 1e6) %>% 
    as.character() %>% 
    ymd()
}

WRC_ALLOC_IRIS_ALL_AUTHS_10_11_17   <- read_csv("../data/WRC_ALLOC_IRIS_ALL_AUTHS_10_11_17.csv") %>% 
  mutate_if(function(x) is.character(x) & length(unique(x)) < 100, factor) %>% 
  mutate_at(vars(ends_with("Date")), fix_dates)

WRC_WISKI_TS_DATA_10_11_17          <- read_csv("../data/WRC_WISKI_TS_DATA_10_11_17.csv") %>% 
  mutate_if(function(x) is.character(x) & length(unique(x)) < 100, factor) %>% 
  mutate_at(vars(ends_with("Date")), fix_dates)

WRC_WISKI_TS_JOIN_TO_AUTHS_10_11_17 <- read_csv("../data/WRC_WISKI_TS_JOIN_TO_AUTHS_10_11_17.csv") %>% 
  mutate_if(function(x) is.character(x) & length(unique(x)) < 100, factor) %>% 
  mutate_at(vars(ends_with("Date")), fix_dates)

datasets <- list(
  WRC_ALLOC_IRIS_ALL_AUTHS_10_11_17 = WRC_ALLOC_IRIS_ALL_AUTHS_10_11_17,
  WRC_WISKI_TS_DATA_10_11_17 = WRC_WISKI_TS_DATA_10_11_17,
  WRC_WISKI_TS_JOIN_TO_AUTHS_10_11_17 = WRC_WISKI_TS_JOIN_TO_AUTHS_10_11_17
  )

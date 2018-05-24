library(feather)

all_claims %>%
  select_if(is.Date) %>%
  names()

all_claims %>%
  select_if(is.numeric) %>%
  names()

all_claims_dt = bind_rows(
  "inst/extdata/all_claims_2016.feather" %>%
    read_feather() %>%
    mutate(dataset = "Injury Claims 2016"),
  "inst/extdata/all_claims_2015.feather" %>%
    read_feather() %>%
    mutate(dataset = "Injury Claims 2015"))

all_claims_dt <- as.data.table(all_claims_dt)


system.time({

  # all_claims_dt[]

  # all_claims_dt$y <- year(   all_claims_dt$AccidentDate)
  # all_claims_dt$m <- month(  all_claims_dt$AccidentDate)
  # all_claims_dt$d <- day(    all_claims_dt$AccidentDate)
  # all_claims_dt$q <- quarter(all_claims_dt$AccidentDate)

  # all_claims_dt$Year        <- ymd(paste0(all_claims_dt$y, "-01-01"))
  # all_claims_dt$Month       <- ymd(paste0(all_claims_dt$y, "-", all_claims_dt$m, "-01"))
  # all_claims_dt$`Six Month` <- ymd(paste0(all_claims_dt$y, "-", ((all_claims_dt$m - 1) %/% 6) * 6 + 1, "-01"))
  # all_claims_dt$Quarter     <- ymd(paste0(all_claims_dt$y, "-", all_claims_dt$q * 3 - 2, "-01"))
  # all_claims_dt$Day         <- ymd(paste0(all_claims_dt$y, "-", all_claims_dt$m, "-", all_claims_dt$d))

  all_claims_dt[
    AccidentDate > min(AccidentDate) & AccidentDate < max(AccidentDate),
    list(ClaimCoststoDateExGST = sum(ClaimCoststoDateExGST)),
    by = list(dataset = dataset, Year = ymd(paste0(year(AccidentDate), "-01-01")))
    ][order(dataset, Year)]

})

system.time({
  all_claims %>%
    mutate(y = year(AccidentDate),
           # m = month(AccidentDate),
           # d = day(AccidentDate),
           # q = quarter(AccidentDate),
           Year        = ymd(paste0(y, "-01-01"))
           # Month       = ymd(paste0(y, "-", m, "-01")),
           # `Six Month` = ymd(paste0(y, "-", ((m - 1) %/% 6) * 6 + 1, "-01")),
           # Quarter     = ymd(paste0(y, "-", q * 3 - 2, "-01")),
           # Day         = ymd(paste0(y, "-", m, "-", d))
           ) %>%
    group_by(dataset, Year) %>%
    summarise(ClaimCoststoDateExGST = sum(ClaimCoststoDateExGST)) %>%
    arrange(Year) %>%
    ungroup()
})

claim_data <- all_claims %>%
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
  group_by_at(c("dataset", input$ts_aggregate)) %>%
  summarise_at(input$select_ts_y, .funs = agg_funcs[input$ts_func][[1]]) %>%
  ungroup()


























































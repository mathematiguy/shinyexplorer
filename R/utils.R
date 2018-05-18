#' @export

dataTableThemed <- function(data) {
  DT::datatable(
    data,
    style = "bootstrap",
    escape = F,
    rownames = FALSE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '50%', targets = "_all")),
      dom = 't',
      pageLength = 10,
      searching = FALSE,
      scrollX = TRUE,
      deferRender = TRUE,
      scroller = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  )
}

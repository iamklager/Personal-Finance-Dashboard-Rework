#### FormatDT
# Function to format a datatable.

FormatDT <- function(df, currency_symbol) {
  res <- DT::datatable(
    data = df[, c("Date", "Product", "Source", "Category", "Amount")],
    style = "bootstrap4", width = "100%", height = "100%", fillContainer = FALSE,
    filter = list(position = "top", clear = FALSE),
    options = list(
      autoWidth = TRUE,
      paging = TRUE,
      pageLength = 5,
      scrollX = FALSE,
      scrollY = TRUE,
      searching = TRUE
    ),
    rownames = FALSE
  )
  res <- DT::formatCurrency(table = res, columns = "Amount", currency = currency_symbol, digits = 2, before = FALSE, rows = NULL)
  res <- DT::formatDate(table = res, columns = "Date", method = "toLocaleDateString", params = NULL, rows = NULL)
  
  return(res)
}
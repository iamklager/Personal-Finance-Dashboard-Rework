#### HCPLSource
# Plotting total profit by source.

HCPLSource <- function(df, dark_mode_on) {
  df <- data.frame(
    amount = round(c(cumsum(df$income)[nrow(df)], -cumsum(df$expenses)[nrow(df)], df$cuminvest[nrow(df)])),
    source = c("income", "expenses", "investments")
  )
  
  res <- highchart()
  res <- hc_plotOptions(res, column = list(borderWidth = 0, grouping = FALSE))
  res <- hc_xAxis(res, title = list(text = "source"), type = "category")
  res <- hc_yAxis(res, title = list(text = "amount"))
  res <- hc_add_series(res, data = df, hcaes(x = source, y = amount, group = source), type = "column")
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}
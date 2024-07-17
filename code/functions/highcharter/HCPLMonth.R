#### HCPLMonth
# Plotting monthly and cumulative profit over time.

HCPLMonth <- function(df, color_pos, color_neg, dark_mode_on) {
  res <- highchart()
  res <- hc_plotOptions(res, column = list(borderWidth = 0, grouping = FALSE, showInLegend = FALSE), 
                        line = list(lineWidth = 1, marker = list(enabledThreshold = 10), showInLegend = TRUE),
                        dataSorting = list(enabled = TRUE))
  res <- hc_xAxis(res, title = list(text = "month"), type = "category")
  res <- hc_yAxis(res, title = list(text = "amount"))
  res <- hc_add_series(res, data = df, hcaes(x = YearMonth, y = Profit),     name = "profit", type = "column", color = color_pos)
  res <- hc_add_series(res, data = df, hcaes(x = YearMonth, y = Loss),       name = "loss",   type = "column", color = color_neg)
  res <- hc_add_series(res, data = df, hcaes(x = YearMonth, y = ProfitLossCum), 
                       name = "profit/loss cumulative", type = "line", color = ifelse(dark_mode_on, "#C0C0C0", "#000000"))
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}
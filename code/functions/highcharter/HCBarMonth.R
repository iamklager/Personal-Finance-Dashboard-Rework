#### HCBarMonth
# A simple highcharter barplot with one color.

HCBarMonth <- function(df, color, name, dark_mode_on) {
  if (nrow(df) == 0) {
    return(
      shiny::validate(
        need((nrow(df) != 0), "No data available based on your selection")
      )
    )
  }
  df$Amount <- round(df$Amount, 2)
  
  res <- highchart()
  res <- hc_plotOptions(res, column = list(borderWidth = 0, grouping = FALSE, showInLegend = FALSE))
  res <- hc_xAxis(res, title = list(text = "month"),  type = "category")
  res <- hc_yAxis(res, title = list(text = "amount"))
  res <- hc_add_series(res, data = df, hcaes(x = YearMonth, y = Amount), type = "column", name = name, color = color)
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}


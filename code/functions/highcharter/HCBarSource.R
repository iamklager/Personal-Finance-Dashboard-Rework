#### HCBarSource
# A simple highcharter barplot colored by source.

HCBarSource <- function(df, dark_mode_on) {
  if (nrow(df) == 0) {
    return(
      shiny::validate(
        need((nrow(df) != 0), "No data available based on your selection")
      )
    )
  }
  
  res <- highchart()
  res <- hc_plotOptions(res, column = list(borderWidth = 0, stacking = "normal"))
  res <- hc_xAxis(res, type = "category")
  res <- hc_yAxis(res, title = list(text = "amount"))
  res <- hc_add_series(res, data = df, hcaes(x = source, y = amount, group = category), type = "column")
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}


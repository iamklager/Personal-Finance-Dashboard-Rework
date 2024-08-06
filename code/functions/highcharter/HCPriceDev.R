#### HCPriceDev
# Plotting the price development of the assets.

HCPriceDev <- function(assets, yearmonth_start, yearmonth_end, dark_mode_on) {
  assets <- dplyr::bind_rows(lapply(assets, function(df) {
    if (nrow(df) == 1) { return(NULL) }
    df$YearMonth <- as.numeric(df$YearMonth)
    df <- df[df$YearMonth >= as.numeric(yearmonth_start) & df$YearMonth <= as.numeric(yearmonth_end), ]
    if (nrow(df) == 0) {
      return(data.frame(date = NULL, asset = NULL, delta = NULL, invested = NULL))
    }
    df$Invested <- ifelse(df$Quantity > 0, "Yes", "No")
    df <- df[, c("Date", "DisplayName", "Adjusted", "Invested")]
    colnames(df) <- c("date", "asset", "delta", "invested")
    df$period <- 1
    for (i in 2:nrow(df)) {
      if (df$invested[i] != df$invested[i - 1]) {
        df$period[i] <- df$period[i - 1] + 1
      } else {
        df$period[i] <- df$period[i - 1]
      }
    }
    df <- split(df, df$period)
    df <- dplyr::bind_rows(lapply(df, function(x) {
      if (all(x$invested == "No")) {
        x$delta <- NA
      } else {
        x$delta <- round(x$delta/x$delta[1] - 1, 3)
      }
      return(x)
    }))
    
    return(df)
  }))
  assets$date <- as.Date(assets$date)
  
  res <- hchart(assets, "line", hcaes(x = date, y = delta, group = asset))
  res <- hc_xAxis(hc = res, type = "datetime")
  res <- hc_yAxis(hc = res, title = list(text = "relative price development"))
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}
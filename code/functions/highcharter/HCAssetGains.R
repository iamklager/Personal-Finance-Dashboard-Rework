#### HCAssetGains
# Plotting the asset gains.

HCAssetGains <- function(assets, yearmonth_start, yearmonth_end, dark_mode_on) {
  assets <- dplyr::bind_rows(lapply(assets, function(df) {
    if (nrow(df) == 1) { return(NULL) }
    df$YearMonth <- as.numeric(df$YearMonth)
    df <- df[df$YearMonth >= as.numeric(yearmonth_start) & df$YearMonth <= as.numeric(yearmonth_end), ]
    if (nrow(df) == 0) {
      return(data.frame(asset = NULL, gain = NULL))
    }
    df$invested <- ifelse(df$Quantity > 0, "Yes", "No")
    df <- df[, c("Date", "DisplayName", "Adjusted", "invested")]
    df$period <- 1
    for (i in 2:nrow(df)) {
      if (df$invested[i] != df$invested[i - 1]) {
        df$period[i] <- df$period[i - 1] + 1
      } else {
        df$period[i] <- df$period[i - 1]
      }
    }
    df <- split(df, df$period)
    df <- df[[length(df)]]
    df$Date <- as.Date(df$Date)
    res <- data.frame(
      asset = df$DisplayName[1],
      gain  = round(100 * (df[df$Date == max(df$Date), "Adjusted"] / df[df$Date == min(df$Date), "Adjusted"]) - 100, 2)
    )
    return(res)
  }))
  
  res <- hchart(assets, "column", hcaes(x = asset, y = gain, group = asset))
  res <- hc_plotOptions(hc = res, column = list(borderWidth = 0, grouping = FALSE))
  res <- hc_yAxis(res, title = list(text = "gains (in %)"))
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}


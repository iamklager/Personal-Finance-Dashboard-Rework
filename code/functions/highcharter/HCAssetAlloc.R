#### HCAssetAlloc
# Plotting the asset allocation either at acquisition price or current price.

PrepAssetAllocData <- function(data, category, at_acquisition, yearmonth_start, yearmonth_end) {
  if(is.null(data)) { return(NULL) }
  data <- dplyr::bind_rows(lapply(data, function(df) {
    df$YearMonth <- as.numeric(df$YearMonth)
    df <- df[df$YearMonth >= as.numeric(yearmonth_start) & df$YearMonth <= as.numeric(yearmonth_end), ]
    df[nrow(df), ]
  }))
  data$category <- category
  if (at_acquisition) {
    data <- data[, c("Date", "PriceTotal", "DisplayName", "category")]
    colnames(data)[2:3] <- c("amount", "asset")
  } else {
    data$amount <- data$Quantity * data$Adjusted
    data <- data[, c("Date", "amount", "DisplayName", "category")]
    colnames(data)[3] <- "asset"
  }
  data$amount <- round(data$amount, 2)
  return(data)
}

HCAssetAlloc <- function(stocks, alternatives, at_acquisition = FALSE, yearmonth_start, yearmonth_end, dark_mode_on) {
  stocks <- PrepAssetAllocData(stocks, "stocks", at_acquisition, yearmonth_start, yearmonth_end)
  alternatives <- PrepAssetAllocData(alternatives, "alternatives", at_acquisition, yearmonth_start, yearmonth_end)
  df <- rbind(stocks, alternatives)
  res <- hchart(df, "column", hcaes(x = category, y = amount, group = asset))
  res <- hc_plotOptions(hc = res, column = list(borderWidth = 0, stacking = "normal", pointPadding = 0, groupPadding = 0.1))
  if (dark_mode_on) {
    res <- hc_add_theme(res, hc_theme_dark())
  } else {
    res <- hc_add_theme(res, hc_theme_light())
  }
  
  return(res)
}


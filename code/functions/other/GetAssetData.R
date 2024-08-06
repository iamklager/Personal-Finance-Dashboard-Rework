#### GetAssetData
# Preprocesses asset data to be used for the plots.

GetAssetData <- function(data, price_data, base_currency) {
  # Breakout condition: The stock dataframe is empty
  if (nrow(data) == 0) { return(NULL) }
  all_symbols <- unique(data$TickerSymbol)
  data$Date <- as.character(data$Date)
  data <- split(data, data$TickerSymbol)
  data <- lapply(all_symbols, function(symbol) {
    asset <- data[[symbol]]
    asset <- asset[order(asset$Date), ]
    asset$Quantity <- as.numeric(asset$Quantity)
    asset$PriceTotal <- as.numeric(asset$PriceTotal)
    asset$Quantity[asset$TransactionType == "Buy"]    <-  1 * asset$Quantity[asset$TransactionType == "Buy"]
    asset$Quantity[asset$TransactionType == "Sell"]   <- -1 * asset$Quantity[asset$TransactionType == "Sell"]
    asset$PriceTotal[asset$TransactionType == "Buy"]  <-  1 * asset$PriceTotal[asset$TransactionType == "Buy"]
    asset$PriceTotal[asset$TransactionType == "Sell"] <- -1 * asset$PriceTotal[asset$TransactionType == "Sell"]
    asset <- merge(x = asset, y = price_data[[symbol]], by = "Date", all = TRUE)
    asset <- asset[which(!is.na(asset$DisplayName))[1]:nrow(asset), ]
    
    asset <- asset[, colnames(asset) %in% c("Date", "YearMonth", "DisplayName", "PriceTotal", "Quantity", "Adjusted")]
    if (nrow(asset) == 1) {
      asset$YearMonth <- format(as.Date(asset$Date), "%Y%m")
      return(asset)
    }
    for (i in 2:nrow(asset)) {
      if (is.na(asset$Quantity[i])) {
        asset$Quantity[i] <- asset$Quantity[i - 1]
        asset$PriceTotal[i] <- asset$PriceTotal[i - 1]
      } else if (asset$Quantity[i] > 0) {
        asset$PriceTotal[i] <- asset$PriceTotal[i] + asset$PriceTotal[i - 1]
        asset$Quantity[i]   <- asset$Quantity[i]   + asset$Quantity[i - 1]
      } else if (asset$Quantity[i] < 0) {
        asset$PriceTotal[i] <- (asset$PriceTotal[i - 1] / asset$Quantity[i - 1]) * (asset$Quantity[i - 1] + asset$Quantity[i])
        asset$Quantity[i]   <- asset$Quantity[i]   + asset$Quantity[i - 1]
      }
      asset[i, is.na(asset[i, ])] <- asset[i - 1, is.na(asset[i, ])]
    }
    asset$YearMonth <- format(as.Date(asset$Date), "%Y%m")
    
    return(asset)
  })
  names(data) <- all_symbols
  
  return(data)
}


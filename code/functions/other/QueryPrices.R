#### QueryPrices
# Function to query the yahoo finance prices for each ticker in an asset table (stocks or alternatives).


QueryPrices <- function(data, path = "data/price_data/stocks/") {
  # Breakout condition: The stock dataframe is empty
  if (nrow(data) == 0) { return(NULL) }
  
  all_symbols <- unique(data$TickerSymbol)
  historic_price_data <- list.files(path)
  today <- as.character(Sys.Date())
  
  price_data <- lapply(all_symbols, function(ticker_symbol) {
    # Today's prices have already been queried
    if (paste0(ticker_symbol, "_", today, ".RDS") %in% historic_price_data) {
      prices <- readRDS(paste0(path, paste0(ticker_symbol, "_", today, ".RDS")))
      return(prices)
    }
    
    # Some historic price data exists
    historic_prices <- grepl(ticker_symbol, historic_price_data)
    if (any(historic_prices)) {
      prices_old <- readRDS(paste0(path, historic_price_data[which(historic_prices)]))
      prices_new <- quantmod::getSymbols(
        Symbols = ticker_symbol, env = NULL, warnings = FALSE, from = prices_old$Date[nrow(prices_old)]
      )
      prices_new <- ggplot2::fortify(prices_new)
      colnames(prices_new) <- sub(paste0("^", ticker_symbol, "."), "", colnames(prices_new))
      colnames(prices_new)[1] <- "Date"
      prices_new <- na.omit(prices_new)
      prices_new$Date <- as.character(prices_new$Date)
      prices <- rbind(prices_old, prices_new)
      saveRDS(prices, paste0(path, ticker_symbol, "_", today, ".RDS"))
      unlink(paste0(path, historic_price_data[which(historic_prices)]))
      return(prices)
    }
    
    # No price data exists
    start_date <- sort(as.character(data[data$TickerSymbol == ticker_symbol, "Date"][[1]]))[1]
    if (format(as.Date(start_date), "%m-%d") == "01-01") {
      start_date <- paste0(as.numeric(format(as.Date(start_date), "%Y")) - 1, "-12-29")
    }
    prices <- quantmod::getSymbols(
      Symbols = ticker_symbol, env = NULL, warnings = FALSE, from = start_date
    )
    prices <- ggplot2::fortify(prices)
    colnames(prices) <- sub(paste0("^", ticker_symbol, "."), "", colnames(prices))
    colnames(prices)[1] <- "Date"
    prices$Date <- as.character(prices$Date)
    prices <- na.omit(prices)
    saveRDS(prices, paste0(path, ticker_symbol, "_", today, ".RDS"))
    return(prices)
  })
  names(price_data) <- all_symbols 
  
  return(price_data)
}

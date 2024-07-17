#### GetXRates
# Query exchange rates to dollar from yahoo.


GetXRates <- function(data, base_currency = "EUR", path = "data/price_data/xrates/") {
  all_currencies <- unique(c(data$TransactionCurrencySymbol, data$SourceCurrencySymbol, base_currency))
  all_currencies <- all_currencies[all_currencies != "USD"]
  historic_xrate_data <- list.files(path)
  today <- as.character(Sys.Date())
  xrate_data <- lapply(all_currencies, function(ticker_symbol) {
    # Today's xrates have already been queried
    if (paste0(ticker_symbol, "_", today, ".RDS") %in% historic_xrate_data) {
      xrates <- readRDS(paste0(path, paste0(ticker_symbol, "_", today, ".RDS")))
      return(xrates)
    }

    # Some historic xrate data exists
    historic_xrates <- grepl(ticker_symbol, historic_xrate_data)
    if (any(historic_xrates)) {
      xrates_old <- readRDS(paste0(path, historic_xrate_data[which(historic_xrates)]))
      xrates_new <- quantmod::getSymbols(
        Symbols = paste0(ticker_symbol, "=X"), env = NULL, warnings = FALSE, from = xrates_old$Date[nrow(xrates_old)]
      )
      xrates_new <- ggplot2::fortify(xrates_new)
      colnames(xrates_new) <- sub(paste0("^",ticker_symbol, "=X."), "", colnames(xrates_new))
      colnames(xrates_new)[1] <- "Date"
      xrates_new <- na.omit(xrates_new)
      xrates_new$Date <- as.character(xrates_new$Date)
      xrates_new <- xrates_new[, c("Date", "Adjusted")]
      for (i in 2:nrow(xrates_new)) {
        if (is.na(xrates_new$Adjusted[i])) {
          xrates_new$Adjusted[i] <- xrates_new$Adjusted[i - 1]
        }
      }
      xrates_new$Adjusted <- 1/xrates_new$Adjusted
      xrates <- rbind(xrates_old, xrates_new)
      saveRDS(xrates, paste0(path, ticker_symbol, "_", today, ".RDS"))
      unlink(paste0(path, historic_xrate_data[which(historic_xrates)]))
      return(xrates)
    }

    # No xrate data exists
    xrates <- quantmod::getSymbols(
      Symbols = paste0(ticker_symbol, "=X"), env = NULL, warnings = FALSE
    )
    xrates <- ggplot2::fortify(xrates)
    colnames(xrates) <- sub(paste0("^",ticker_symbol, "=X."), "", colnames(xrates))
    colnames(xrates)[1] <- "Date"
    xrates$Date <- as.character(xrates$Date)
    xrates <- xrates[, c("Date", "Adjusted")]
    for (i in 2:nrow(xrates)) {
      if (is.na(xrates$Adjusted[i])) {
        xrates$Adjusted[i] <- xrates$Adjusted[i - 1]
      }
    }
    xrates$Adjusted <- 1/xrates$Adjusted
    saveRDS(xrates, paste0(path, ticker_symbol, "_", today, ".RDS"))
    return(xrates)
  })
  names(xrate_data) <- paste0(all_currencies, "/USD")
  return(xrate_data)
}



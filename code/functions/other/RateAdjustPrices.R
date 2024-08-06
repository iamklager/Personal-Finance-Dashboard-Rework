#### RateAdjustPrices
# Function to convert asset prices to the user's base currency (first converts to USD and then to base currency).


RateAdjustPrices <- function(df, currency_symbol, base_currency, xrates) {
  if (currency_symbol == c_BaseCurrency) {
    if (nrow(df) == 1) {
      return(df)
    } else {
      for (i in (which(!all(is.na(df[, c("Open", "High", "Low", "Close", "Volume", "Adjusted")])))[1] + 1):nrow(df)) {
        if (all(is.na(df[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted")]))) {
          df[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted")] <- df[i - 1, c("Open", "High", "Low", "Close", "Volume", "Adjusted")]
        }
      }
    }
    df <- df[df$Date >= min(df$Date), ]
    return(df)
  } else if (currency_symbol == "USD") {
    res <- merge(
      x = df, y = xrates[[paste0(base_currency, "/USD")]][, c("Date", "Adjusted")], 
      by.x = "Date", by.y = "Date", all.x = TRUE, all.y = TRUE
    )
    if (nrow(df) == 1) {
      for (i in 2:nrow(res)) {
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
    } else {
      for (i in (which(!all(is.na(res[, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")])))[1] + 1):nrow(res)) {
        if (all(is.na(res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]))) {
          res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")] <- res[i - 1, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]
        }
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
      res <- res[res$Date >= min(df$Date), ]
    }
    res <- na.omit(res)
    res[, c("Open", "High", "Low", "Close", "Adjusted.x")] <- res[, c("Open", "High", "Low", "Close", "Adjusted.x")] / res[, c("Adjusted.y")]
    res <- res[, 1:(ncol(res) - 1)]
    colnames(res)[ncol(res)] <- "Adjusted"
    return(res)
  } else {
    # Converting to USD
    res <- merge(
      x = df, y = xrates[[paste0(currency_symbol, "/USD")]][, c("Date", "Adjusted")], 
      by.x = "Date", by.y = "Date", all.x = TRUE, all.y = TRUE
    )
    if (nrow(df) == 1) {
      for (i in 2:nrow(res)) {
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
    } else {
      for (i in (which(!all(is.na(res[, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")])))[1] + 1):nrow(res)) {
        if (all(is.na(res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]))) {
          res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")] <- res[i - 1, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]
        }
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
      res <- res[res$Date >= min(df$Date), ]
    }
    res <- na.omit(res)
    res[, c("Open", "High", "Low", "Close", "Adjusted.x")] <- res[, c("Open", "High", "Low", "Close", "Adjusted.x")] / res[, c("Adjusted.y")]
    res <- res[, 1:(ncol(res) - 1)]
    colnames(res)[ncol(res)] <- "Adjusted"
    # Converting to base currency
    if (base_currency == "USD") {
      return(res)
    }
    res <- merge(
      x = res, y = xrates[[paste0(base_currency, "/USD")]][, c("Date", "Adjusted")], 
      by.x = "Date", by.y = "Date", all.x = TRUE, all.y = TRUE
    )
    if (nrow(df) == 1) {
      for (i in 2:nrow(res)) {
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
    } else {
      for (i in (which(!all(is.na(res[, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")])))[1] + 1):nrow(res)) {
        if (all(is.na(res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]))) {
          res[i, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")] <- res[i - 1, c("Open", "High", "Low", "Close", "Volume", "Adjusted.x")]
        }
        if (is.na(res$Adjusted.y[i])) {
          res$Adjusted.y[i] <- res$Adjusted.y[i - 1]
        }
      }
      res <- res[res$Date >= min(df$Date), ]
    }
    res <- na.omit(res)
    res[, c("Open", "High", "Low", "Close", "Adjusted.x")] <- res[, c("Open", "High", "Low", "Close", "Adjusted.x")] / res[, c("Adjusted.y")]
    res <- res[, 1:(ncol(res) - 1)]
    colnames(res)[ncol(res)] <- "Adjusted"
    return(res)
  }
}
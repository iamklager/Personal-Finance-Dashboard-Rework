#### AssetPLMonth
# Computes monthly profit and loss for a list of assets.


AssetPLMonth <- function(assets, yearmonth_start, yearmonth_end) {
  
  ## By asset
  assets <- lapply(assets, function(asset) {
    asset <- asset[
      as.numeric(asset$YearMonth) >= yearmonth_start & as.numeric(asset$YearMonth) <= yearmonth_end,
    ]
    asset <- na.omit(asset)
    if (nrow(asset) == 0) {
      res <- data.frame(
        Date = NULL,
        YearMonth = NULL,
        DisplayName = NULL,
        ProfitLoss = NULL,
        CumProfitLoss = NULL
      )
      return(res)
    } else if (nrow(asset) == 1) {
      res <- data.frame(
        Date = asset$Date[1],
        YearMonth = asset$YearMont[1],
        DisplayName = asset$DisplayName[1],
        ProfitLoss = 0,
        CumProfitLoss = 0
      )
      return(res)
    }
    
    ## Cumulative profit/loss
    asset$CumProfitLoss <- 0
    asset$NewQuantity <- asset$Quantity[1]
    asset$NewPrice <- asset$PriceTotal[1]
    zero_index <- 1
    for (i in 2:nrow(asset)) {
      if (asset$Quantity[i] == 0) {
        asset$NewQuantity[1:i] <- 0
        asset$NewPrice[1:i] <- 0
        zero_index <- i + 1
      } else if (asset$Quantity[i] < asset$Quantity[i-1]) {
        asset$NewQuantity[zero_index:i] <- asset$Quantity[i]
        asset$NewPrice[zero_index:i] <- asset$PriceTotal[i]
      } else {
        asset$NewQuantity[i] <- asset$Quantity[i]
        asset$NewPrice[i] <- asset$PriceTotal[i]
      }
      asset$CumProfitLoss[i] <- sum(c(0, diff(asset$Adjusted[1:i])) * asset$NewQuantity[1:i]) + 
        ((asset$Adjusted[1] * asset$NewQuantity[1]) - asset$NewPrice[1])
    }
    asset <- asset[, c("Date", "YearMonth", "DisplayName", "CumProfitLoss")]
    
    ## Monthly profit/loss 
    asset  <- split(asset, asset$YearMonth)
    asset <- dplyr::bind_rows(lapply(asset, function(df) { return(df[nrow(df), ]) }))
    asset$ProfitLoss <- c(asset$CumProfitLoss[1], diff(asset$CumProfitLoss))
    
    return(asset)
  })
  
  return(assets)
}


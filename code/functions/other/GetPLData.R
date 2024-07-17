#### GetPLData
# Function to get monthly profit and loss data.


GetPLData <- function(income, expenses, assets) {
  if (nrow(income) > 0) {
    income <- aggregate.data.frame(income$Amount, list(income$YearMonth), sum)
    colnames(income) <- c("YearMonth", "income")
  } else {
    income <- data.frame(
      YearMonth = unique(unlist(lapply(assets, function(asset) { asset$YearMonth }))),
      income = 0
    )
  }
  if (nrow(expenses) > 0) {
    expenses$Amount <- expenses$Amount
    expenses <- aggregate.data.frame(expenses$Amount, list(expenses$YearMonth), sum)
    colnames(expenses) <- c("YearMonth", "expenses")
  } else {
    expenses <- data.frame(
      YearMonth = unique(unlist(lapply(assets, function(asset) { asset$YearMonth }))),
      expenses = 0
    )
  }
  if (nrow(expenses) > 0 | nrow(income) > 0) {
    df <- merge(income, expenses, all = TRUE)
    df[is.na(df)]  <- 0
    df$unrealizedinvestments <- 0
    df$cuminvest <- 0
  } else {
    df <- data.frame(
      YearMonth = NULL,
      income = NULL,
      expenses = NULL,
      unrealizedinvestments = NULL,
      cuminvest = NULL
    )
  }
  if (length(assets) != 0) {
    for (asset in assets) {
      if (nrow(asset) == 0) {
        next
      }
      for (i in 1:nrow(asset)) {
        if ((nrow(df) >= 0) & (asset$YearMonth[i] %in% df$YearMonth)) {
          df[df$YearMonth == asset$YearMonth[i], "unrealizedinvestments"] <- df[df$YearMonth == asset$YearMonth[i], "unrealizedinvestments"] + asset$ProfitLoss[i]
          df[df$YearMonth == asset$YearMonth[i], "cuminvest"] <- df[df$YearMonth == asset$YearMonth[i], "cuminvest"] + asset$CumProfitLoss[i]
        } else {
          df <- rbind(
            df, 
            data.frame(
              YearMonth = asset$YearMonth[i], 
              income    = 0, 
              expenses  = 0, 
              unrealizedinvestments = asset$ProfitLoss[i],
              cuminvest = asset$CumProfitLoss[i]
            )
          )
        }
      }
    }
  }
  df <- na.omit(df)
  df$ProfitLoss <- df$income - df$expenses + df$unrealizedinvestments
  df$ProfitLoss <- round(df$ProfitLoss, 2)
  df$cuminvest <- round(df$cuminvest, 2)
  df$Profit <- ifelse(df$ProfitLoss >= 0, df$ProfitLoss, 0)
  df$Loss   <- ifelse(df$ProfitLoss <  0, df$ProfitLoss, 0)
  df <- df[order(df$YearMonth), ]
  df$ProfitLossCum <- round(cumsum(df$income - df$expenses) + df$cuminvest , 2) # This is actually pretty smart.. I wish I had thought about it sooner
  
  return(df)
}
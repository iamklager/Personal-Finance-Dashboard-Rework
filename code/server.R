#### Server
# The server...


server <- function(input, output, session) {
  
  ### Self refreshing data (refresh after n_RefreshTime hours)
  # Raw data
  income_raw <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60 * n_RefreshTime,
    session        = session,
    filePath       = "data/income.xlsx",
    readFunc       = readxl::read_xlsx
  )
  expenses_raw <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60 * n_RefreshTime,
    session        = session,
    filePath       = "data/expenses.xlsx",
    readFunc       = readxl::read_xlsx
  )
  stocks_raw <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60 * n_RefreshTime,
    session        = session,
    filePath       = "data/stocks.xlsx",
    readFunc       = readxl::read_xlsx
  )
  stocks_pre <- reactive({ stocks_raw()[stocks_raw()$Date != Sys.Date(), ] })
  alternatives_raw <- reactiveFileReader(
    intervalMillis = 1000 * 60 * 60 * n_RefreshTime,
    session        = session,
    filePath       = "data/alternatives.xlsx",
    readFunc       = readxl::read_xlsx
  )
  alternatives_pre <- reactive({ alternatives_raw()[alternatives_raw()$Date != Sys.Date(), ] })
  # Exchange rates
  xrates_stocks <- reactive({
    invalidateLater(1000 * 60 * 60 * n_RefreshTime)
    GetXRates(stocks_pre(), c_BaseCurrency, "data/price_data/xrates/")
  })
  xrates_alternatives <- reactive({
    invalidateLater(1000 * 60 * 60 * n_RefreshTime)
    GetXRates(alternatives_pre(), c_BaseCurrency, "data/price_data/xrates/")
  })
  
  
  ### Reactive values
  # Income
  income_pre <- reactive({
    res <- income_raw()
    if (nrow(income_raw()) != 0) {
      res$Amount[is.na(res$Amount)] <- 0
      for (cname in colnames(res)[2:ncol(res)]) {
        if (cname != "Amount") { res[is.na(res[, cname]), cname] <- "-" }
      }
      res$YearMonth <- format(res$Date, "%Y%m")
    }
    res
  })
  income_month <- reactive({
    if (nrow(income_pre()) != 0) {
      res <- aggregate.data.frame(income_pre()[, "Amount"], list(income_pre()$YearMonth), sum)
      colnames(res) <- c("YearMonth", "Amount")
      res$Year <- as.numeric(substr(res$YearMonth, 1, 4))
      res$Month <- as.numeric(substr(res$YearMonth, 5, 6))
    } else {
      res <- income_pre()
    }
    res
  })
  income_cat_source <- reactive({
    res <- income_pre()
    if (nrow(income_pre()) != 0) {
      res <- aggregate.data.frame(
        x   = income_pre()[, "Amount"], 
        by  = list(income_pre()$YearMonth, income_pre()$Source, income_pre()$Category),
        FUN = sum
      )
      colnames(res) <- c("YearMonth", "source", "category", "amount")
      res$amount <- round(res$amount, 2)
    }
    res
  })
  # Expenses
  expenses_pre <- reactive({
    res <- expenses_raw()
    if (nrow(expenses_raw()) != 0) {
      res$Amount[is.na(res$Amount)] <- 0
      for (cname in colnames(res)[2:ncol(res)]) {
        if (cname != "Amount") { res[is.na(res[, cname]), cname] <- "-" }
      }
      res$YearMonth <- format(res$Date, "%Y%m")
    }
    res
  })
  expenses_month <- reactive({
    if (nrow(expenses_pre()) != 0) {
      res <- aggregate.data.frame(expenses_pre()[, "Amount"], list(expenses_pre()$YearMonth), sum)
      colnames(res) <- c("YearMonth", "Amount")
    }
    res
  })
  expenses_cat_source <- reactive({
    res <- expenses_pre()
    if (nrow(expenses_pre()) != 0) {
      res <- aggregate.data.frame(
        x   = expenses_pre()[, "Amount"], 
        by  = list(expenses_pre()$YearMonth, expenses_pre()$Source, expenses_pre()$Category),
        FUN = sum
      )
      colnames(res) <- c("YearMonth", "source", "category", "amount")
      res$amount <- round(res$amount, 2)
    }
    res
  })
  # Assets
  stocks_prices <- reactive({
    res <- QueryPrices(data = stocks_pre(), path = "data/price_data/stocks/")
    res_names <- names(res)
    if (!is.null(res)) {
      res <- lapply(res_names, function(name) {
        df <- res[[name]]
        currency_symbol <- unlist(stocks_pre()[stocks_pre()$TickerSymbol == name, "SourceCurrencySymbol"][1, 1])
        res <- RateAdjustPrices(df, currency_symbol, c_BaseCurrency, xrates_stocks())
        return(res)
      })
    }
    names(res) <- res_names
    res
  })
  alternatives_prices <- reactive({
    res <- QueryPrices(data = alternatives_pre(), path = "data/price_data/alternatives/")
    res_names <- names(res)
    if (!is.null(res)) {
      res <- lapply(res_names, function(name) {
        df <- res[[name]]
        currency_symbol <- unlist(alternatives_pre()[alternatives_pre()$TickerSymbol == name, "SourceCurrencySymbol"][1, 1])
        res <- RateAdjustPrices(df, currency_symbol, c_BaseCurrency, xrates_alternatives())
        return(res)
      })
    }
    names(res) <- res_names
    res
  })
  stocks_data <- reactive({
    GetAssetData(stocks_pre(), stocks_prices(), c_BaseCurrency)
  })
  alternatives_data <- reactive({
    GetAssetData(alternatives_pre(), alternatives_prices(), c_BaseCurrency)
  })
  # Misc
  all_years <- reactive({
    as.character(min(
      ifelse(nrow(income_raw())       == 0, as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(income_raw()$Date, "%Y"))      ),
      ifelse(nrow(expenses_raw())     == 0, as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(expenses_raw()$Date, "%Y"))    ),
      ifelse(nrow(stocks_pre())       == 0, as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(stocks_pre()$Date, "%Y"))      ),
      ifelse(nrow(alternatives_pre()) == 0, as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(alternatives_pre()$Date, "%Y")))
    ):as.numeric(format(Sys.Date(), "%Y")))
  })
  # Profit/Loss
  asset_pl_month <- reactive({
    AssetPLMonth(
      assets = c(stocks_data(), alternatives_data()), 
      yearmonth_start = as.numeric(paste0(input$input_YearStart, input$input_MonthStart)), 
      yearmonth_end   = as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd))
    )
  })
  pl_df_month <- reactive({
    GetPLData(
      income = income_pre()[
        as.numeric(income_pre()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) &
          as.numeric(income_pre()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
      ],
      expenses = expenses_pre()[
        as.numeric(expenses_pre()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) &
          as.numeric(expenses_pre()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
      ],
      assets = asset_pl_month()
    )
  })
  
  
  ### Event handling
  observe({
    updatePickerInput(inputId = "input_YearStart", choices = all_years(), selected = as.numeric(format(Sys.Date(), "%Y")))
    updatePickerInput(inputId = "input_YearEnd",   choices = all_years(), selected = as.numeric(format(Sys.Date(), "%Y")))
  })
  observeEvent(
    eventExpr   = { input$input_DarkMode },
    handlerExpr = {ifelse(
      input$input_DarkMode,
      toggle_dark_mode(mode = "dark"),
      toggle_dark_mode(mode = "light")
    )}
  )
  
  
  ### Outputs
  # Summary
  output$t_ProfitTotal <- renderUI({
    res   <- round(pl_df_month()$ProfitLossCum[nrow(pl_df_month())], 2)
    color <- ifelse(res >= 0, c_ColorPos, c_ColorNeg)
    res   <- as.character(res)
    dot_pos <-  unlist(gregexpr("\\.", res))
    if (dot_pos > 4) {
      res <- paste0(
        substr(res, 1, dot_pos - 4),
        " ",
        substr(res, dot_pos - 3, nchar(res))
      )
    }
    res   <- paste0(res, " ", c_CurrencySymbol)
    shiny::tags$span(
      style = paste0(
        "color:", color, "; ",
        "margin: auto; text-align: center; ",
        "font-size: 96px;"
      ),
      res
    )
  })
  output$t_ProfitPercIncome <- renderUI({
    res <- round(100 * (pl_df_month()$ProfitLossCum[nrow(pl_df_month())] / cumsum(pl_df_month()$income)[nrow(pl_df_month())]), 2)
    if (is.infinite(res)) {
      color <- "#C0C0C0"
      res   <- "No recorded income."
    } else {
      color <- ifelse(res >= 0, c_ColorPos, c_ColorNeg)
      res   <- as.character(res)
      dot_pos <-  unlist(gregexpr("\\.", res))
      if (dot_pos > 4) {
        res <- paste0(
          substr(res, 1, dot_pos - 4),
          " ",
          substr(res, dot_pos - 3, nchar(res))
        )
      }
      res   <- paste0(res, " %")
    }
    shiny::tags$span(
      style = paste0(
        "color:", color, "; ",
        "margin: auto; text-align: center; ",
        "font-size: 96px;"
      ),
      res
    )
  })
  output$p_ProfitSource <- renderHighchart({
    HCPLSource(df = pl_df_month(), dark_mode_on = input$input_DarkMode)
  })
  output$p_ProfitMonth <- renderHighchart({
    HCPLMonth(df = pl_df_month(), color_pos = c_ColorPos, color_neg = c_ColorNeg, dark_mode_on = input$input_DarkMode)
  })
  # Income
  output$p_IncomeCategory <- renderHighchart({
    shiny::validate(need((nrow(income_cat_source()) != 0), "No income data available."))
    HCBarCategory(income_cat_source()[
      as.numeric(income_cat_source()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
      as.numeric(income_cat_source()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], dark_mode_on = input$input_DarkMode)
  })
  output$p_IncomeMonth <- renderHighchart({
    shiny::validate(need((nrow(income_month()) != 0), "No income data available."))
    HCBarMonth(income_month()[
      as.numeric(income_month()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
      as.numeric(income_month()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], color = c_ColorPos, name = "income", dark_mode_on = input$input_DarkMode)
  })
  output$p_IncomeSource <- renderHighchart({
    shiny::validate(need((nrow(income_cat_source()) != 0), "No income data available."))
    HCBarSource(income_cat_source()[
      as.numeric(income_cat_source()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(income_cat_source()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], dark_mode_on = input$input_DarkMode)
  })
  output$p_IncomeSankey <- renderHighchart({
    shiny::validate(need((nrow(income_raw()) != 0), "No income data available."))
    HCSankey(income_pre()[
      as.numeric(income_pre()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(income_pre()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], "income", dark_mode_on = input$input_DarkMode)
  })
  output$t_IncomeItems <- DT::renderDT({
    FormatDT(income_pre(), c_CurrencySymbol)
  })
  # Expenses
  output$p_ExpensesCategory <- renderHighchart({
    HCBarCategory(expenses_cat_source()[
      as.numeric(expenses_cat_source()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(expenses_cat_source()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], dark_mode_on = input$input_DarkMode)
  })
  output$p_ExpensesMonth <- renderHighchart({
    HCBarMonth(expenses_month()[
      as.numeric(expenses_month()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(expenses_month()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], color = c_ColorNeg, name = "expenses", dark_mode_on = input$input_DarkMode)
  })
  output$p_ExpensesSource <- renderHighchart({
    HCBarSource(expenses_cat_source()[
      as.numeric(expenses_cat_source()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(expenses_cat_source()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], dark_mode_on = input$input_DarkMode)
  })
  output$p_ExpensesSankey <- renderHighchart({
    shiny::validate(need((nrow(expenses_raw()) != 0), "No income data available."))
    HCSankey(expenses_pre()[
      as.numeric(expenses_pre()$YearMonth) >= as.numeric(paste0(input$input_YearStart, input$input_MonthStart)) & 
        as.numeric(expenses_pre()$YearMonth) <= as.numeric(paste0(input$input_YearEnd, input$input_MonthEnd)),
    ], "expenses", dark_mode_on = input$input_DarkMode)
  })
  output$t_ExpensesItems <- DT::renderDT({
    FormatDT(expenses_pre(), c_CurrencySymbol)
  })
  # Assets
  output$p_AssetAllocAcq <- renderHighchart({
    shiny::validate(need(!(is.null(stocks_data()) & is.null(alternatives_data())), "No assets in the portfolio."))
    HCAssetAlloc(
      stocks = stocks_data(), alternatives = alternatives_data(), at_acquisition = TRUE,
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
  output$p_AssetAllocCur <- renderHighchart({
    shiny::validate(need(!(is.null(stocks_data()) & is.null(alternatives_data())), "No assets in the portfolio."))
    HCAssetAlloc(
      stocks = stocks_data(), alternatives = alternatives_data(), at_acquisition = FALSE,
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
  output$p_AssetGainStocks <- renderHighchart({
    shiny::validate(need(!is.null(stocks_data()), "No stocks in the portfolio."))
    HCAssetGains(
      stocks_data(),
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
  output$p_AssetGainAlternatives <- renderHighchart({
    shiny::validate(need(!is.null(alternatives_data()), "No alternatives in the portfolio."))
    HCAssetGains(
      alternatives_data(),
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
  output$p_PriceDevStocks <- renderHighchart({
    shiny::validate(need(!is.null(stocks_data()), "No stocks in the portfolio."))
    HCPriceDev(
      stocks_data(),
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
  output$p_PriceDevAlternatives <- renderHighchart({
    shiny::validate(need(!is.null(alternatives_data()), "No alternatives in the portfolio."))
    HCPriceDev(
      alternatives_data(),
      yearmonth_start = paste0(input$input_YearStart, input$input_MonthStart),
      yearmonth_end = paste0(input$input_YearEnd, input$input_MonthEnd), 
      dark_mode_on = input$input_DarkMode
    )
  })
}



#### UI
# The ui...


ui <- navbarPage(
  ## Design stuff
  theme = bs_theme(
    # Base theme
    preset     = "cosmo",
    primary    = "gray"
  ),
  title = "My Finances",
  windowTitle = "My Finances",
  selected = "Summary",
  lang = "en",
  
  sidebar = sidebar(
    switchInput(
      inputId  = "input_DarkMode",
      label    = "",
      value    = c_DarkOnStart,
      onLabel  = "light",
      offLabel = "dark"
    ),
    div(
      style = "display: flex; gap: 20px;",
      pickerInput(
        inputId = "input_YearStart",
        label = "Start Year",
        choices = as.numeric(format(Sys.Date(), "%Y")),
        selected = as.numeric(format(Sys.Date(), "%Y"))
      ),
      pickerInput(
        inputId = "input_MonthStart",
        label = "Start Month",
        choices = c(paste0(0, 1:9), as.character(10:12)),
        selected = "01"
      )
    ),
    div(
      style = "display: flex; gap: 20px;",
      pickerInput(
        inputId = "input_YearEnd",
        label = "End Year",
        choices = format(Sys.Date(), "%Y"),
        selected = format(Sys.Date(), "%Y")
      ),
      pickerInput(
        inputId = "input_MonthEnd",
        label = "End Month",
        choices = c(paste0(0, 1:9), as.character(10:12)),
        selected = ifelse(nchar(format(Sys.Date(), "%m")) == 1, paste0(0, format(Sys.Date(), "%m")), format(Sys.Date(), "%m"))
      )
    ),
    tooltip(bsicons::bs_icon("info-circle"), l_ToolTips[["ToolTips"]], placement = "right"),
  ),
  
  nav_panel(
    title = "Summary",
    layout_columns(
      navset_card_underline(
        title = "Profit/loss (total)" |> tooltip(l_ToolTips[["ProfitTotal"]]),
        full_screen = TRUE,
        nav_panel(title = "", uiOutput("t_ProfitTotal"))
      ) ,
      navset_card_underline(
        title = "Profit/loss (as % of income)" |> tooltip(l_ToolTips[["ProfitPercIncome"]]),
        full_screen = TRUE,
        nav_panel(title = "", uiOutput("t_ProfitPercIncome"))
      )
    ),
    layout_columns(
      navset_card_underline(
        title = "Income, expenses, and investments" |> tooltip(l_ToolTips[["ProfitSource"]]),
        full_screen = TRUE,
        nav_panel(title = "", highchartOutput("p_ProfitSource"))
      ),
      navset_card_underline(
        title = "Profit/loss over time" |> tooltip(l_ToolTips[["ProfitMonth"]]),
        full_screen = TRUE,
        nav_panel(title = "", highchartOutput("p_ProfitMonth"))
      )
    )
  ),
  
  nav_panel(
    title = "Income",
    layout_columns(
      navset_card_underline(
        title = "Income by category" |> tooltip(l_ToolTips[["IncomeCategory"]]),
        nav_panel(title = "", highchartOutput("p_IncomeCategory")),
        full_screen = TRUE
      ),
      navset_card_underline(
        title = "Income over time" |> tooltip(l_ToolTips[["IncomeMonth"]]),
        nav_panel(title = "", highchartOutput("p_IncomeMonth")),
        full_screen = TRUE
      )
    ),
    layout_columns(
      navset_card_underline(
        title = "Income by source" |> tooltip(l_ToolTips[["IncomeSource"]]),
        nav_panel(title = "", highchartOutput("p_IncomeSource")),
        full_screen = TRUE
      )
    ),
    layout_columns(
      navset_card_underline(
        title = "Income sankey" |> tooltip(l_ToolTips[["IncomeSankey"]]),
        nav_panel(title = "", highchartOutput("p_IncomeSankey")),
        full_screen = TRUE
      )
    ),
    navset_card_underline(
      title = "Income items" |> tooltip(l_ToolTips[["IncomeItems"]]),
      nav_panel(title = "", DT::DTOutput("t_IncomeItems")),
      full_screen = TRUE
    )
  ),
  
  nav_panel(
    title = "Expenses",
    layout_columns(
      navset_card_underline(
        title = "Expenses by category" |> tooltip(l_ToolTips[["ExpensesCategory"]]),
        nav_panel(title = "", highchartOutput("p_ExpensesCategory")),
        full_screen = TRUE
      ),
      navset_card_underline(
        title = "Expenses over time" |> tooltip(l_ToolTips[["ExpensesMonth"]]),
        nav_panel(title = "", highchartOutput("p_ExpensesMonth")),
        full_screen = TRUE
      )
    ),
    navset_card_underline(
      title = "Expenses by source" |> tooltip(l_ToolTips[["ExpensesSource"]]),
      nav_panel(title = "", highchartOutput("p_ExpensesSource")),
      full_screen = TRUE
    ),
    navset_card_underline(
      title = "Expenses sankey" |> tooltip(l_ToolTips[["ExpensesSankey"]]),
      nav_panel(title = "", highchartOutput("p_ExpensesSankey")),
      full_screen = TRUE
    ),
    navset_card_underline(
      title = "Expenses items" |> tooltip(l_ToolTips[["ExpensesItems"]]),
      nav_panel(title = "", DT::DTOutput("t_ExpensesItems")),
      full_screen = TRUE
    )
  ),
  
  nav_panel(
    title = "Assets",
    layout_columns(
      navset_card_underline(
        title = "Asset allocation" |> tooltip(l_ToolTips[["AssetAlloc"]]),
        full_screen = TRUE,
        nav_panel(title = "Current value", highchartOutput("p_AssetAllocCur")),
        nav_panel(title = "Acquisition value", highchartOutput("p_AssetAllocAcq"))
      ),
      navset_card_underline(
        title = "Asset gains" |> tooltip(l_ToolTips[["AssetGain"]]),
        full_screen = TRUE,
        nav_panel(title = "Stocks", highchartOutput("p_AssetGainStocks")),
        nav_panel(title = "Alternatives", highchartOutput("p_AssetGainAlternatives"))
      )
    ),
    navset_card_underline(
      title = "Price development" |> tooltip(l_ToolTips[["PriceDev"]]),
      full_screen = TRUE,
      nav_panel(title = "Stocks", highchartOutput("p_PriceDevStocks")),
      nav_panel(title = "Alternatives", highchartOutput("p_PriceDevAlternatives"))
    )
  )
  
)
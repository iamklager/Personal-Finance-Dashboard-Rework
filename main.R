#### Main
# Run this file to start the app.


### Libraries ----
library(bsicons)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(highcharter)
library(quantmod)
library(readxl)
library(reshape2)
library(shiny)
library(shinyWidgets)


### Functions ----
for (file in list.files("code/functions/highcharter")) { source(paste0("code/functions/highcharter/", file)) }
for (file in list.files("code/functions/other")      ) { source(paste0("code/functions/other/", file))       }
source("code/tooltips.R")


### Constants ----
n_RefreshTime    <- 1              # Rate in which data is refreshed (in hours)
c_BaseCurrency   <- "EUR"          # The currency in which things should be displayed (i.e., the currency in which expenses and income are tracked)
c_CurrencySymbol <- "€"            # Symbol of the displayed currency
c_ColorPos       <- "#90ed7d"      # The color for profits
c_ColorNeg       <- "#f45b5b"      # The color for losses
c_DarkOnStart    <- FALSE          # Start the app in dark mode or not


### Server and UI ----
source("code/ui.R")
source("code/server.R")


### Run the app ----
shinyApp(ui = ui, server = server)


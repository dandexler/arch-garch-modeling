#################################################
# Financial Analytics II
# David Andexler
# Team 11 - Modeling Volatility
#################################################

library(graphics)
library(quantmod)
library(TTR)
library(dplyr)
library(ks)
library(scales)
library(forecast)
library(aTSA)
library(ccgarch) # Removed from CRAN
library(fGarch)
library(rugarch)
library(rvest)
library(stringr)

#################################################
#
# Retrieving and Posturing Data
#
#################################################


# Reads in Ticker Symbols for DJIA from Wikipedia HTML table
rm(list = ls())
tbl <- read_html('https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average') %>% html_nodes(xpath = '//*[@id="constituents"]') # XPath obtained from 'Inspect' option in browser
tbl <- tbl[1] %>% html_table() %>% as.data.frame() # Converts the HTML table into a DataFrame
tbl$symbol_simple <-  tbl$Symbol %>% str_remove("NYSE:") # Strips NYSE: from ticker symbols
tbl$symbol_simple <- str_trim(tbl$symbol_simple, side = 'left') # Removes leading space
tickers <- tbl$symbol_simple # Saves as ticker

# For each ticker symbol, gathers historical data from 2011-03-01 through 2020-03-03
for (i in 1:length(tickers)) {
  if (i == 1) { # If the first symbol, initializes the data frame
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04') # Gets historical data
    df <- eval(as.name(paste(tickers[i]))) # Calls the variable form of the ticker symbol based on index
    stocks_close_df <- data.frame(df[,4]) # Pulls off closing price
  }
  else { # Same function as above, just for all other indices
    print(paste("Starting", tickers[i]))
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04')
    df2 <- eval(as.name(paste(tickers[i])))
    stock_df <- df2[,4]
    stocks_close_df <- cbind.xts(stocks_close_df, stock_df, fill = NA) # Appends to previous dataframe
  }

}

# Strange bug in first column, MMM, no time to fix

# Replaces first column with correct closing prices
stocks_close_df[,1] <- MMM[,4]
colnames(stocks_close_df)[1] <- "MMM.Close"

# Calculates daily return for stocks
for (column in colnames(stocks_close_df)) {
  stocks_close_df[,column] <- ROC(stocks_close_df[,column])
}

# Adjusts column names to '{Stock Name}_r'
col_list <- colnames(stocks_close_df)
for (i in 1:length(col_list)) {
  col_list[i] <- paste(str_remove(col_list[i], '.Close'), '_r', sep = "")
}
colnames(stocks_close_df) <- col_list

# Saves CSV of final data frame
setwd('#')
write.csv(stocks_close_df, 'stocks.csv')

stocks <- data.frame(stocks_close_df)
plot(stocks$AAPL_r, col="black", main="AAPL Stock Return (Logarithmic)", xlab="", ylab="Daily Returns", lwd=2, type="l")

#################################################################
# Analysis
#################################################################

# Testing for ARCH effects with Lagrange Multiplier 1 Lag Test
for (column in colnames(stocks)) {
  print(paste('\nStock: ', column))
  arch.test(arima(stocks[, column ][-1], order = c(0,0,0)), output = TRUE)
}

# Will need to pull p-values from the above and select the top 5 most significant
# Pick one of the following models for each of the top 5: GARCH(1,1)- Normal, t-GARCH(1,1), QGARCH(1,1)-Normal, QGARCH(1,1)-t
# Select based on lowest BIC value
# Forecast next 30 days of volatility
# Rank in order of effect of market shock
# Rank in order of persistence of market shock

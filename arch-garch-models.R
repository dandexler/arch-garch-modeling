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
library(rowr)

rm(list = ls())
tbl <- read_html('https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average') %>% html_nodes(xpath = '//*[@id="constituents"]')
tbl <- tbl[1] %>% html_table() %>% as.data.frame()
tbl$symbol_simple <-  tbl$Symbol %>% str_remove("NYSE:")
tbl$symbol_simple <- str_trim(tbl$symbol_simple, side = 'left')
tickers <- tbl$symbol_simple

for (i in 1:length(tickers)) {
  if (i == 1) {
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04')
    df <- eval(as.name(paste(tickers[i])))
    stocks_close_df <- data.frame(df[,4])
    print(paste("Iteration", i, tickers[i], "Complete"))
  }
  else {
    print(paste("Starting", tickers[i]))
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04')
    print("Break 1")
    df2 <- eval(as.name(paste(tickers[i])))
    print("Break 2")
    
    stock_df <- df2[,4]
    print("Break 3")
    stocks_close_df <- cbind.xts(stocks_close_df, stock_df, fill = NA)
    print("Break 4")
    
    print(paste("Iteration", i, tickers[i], "Complete"))
  }

}

# Strange bug in first column, MMM, no time to fix

# Replaces first column with correct closing prices
stocks_close_df[,1] <- MMM[,4]
colnames(stocks_close_df)[1] <- "MMM.Close"

setwd('C:/Users/dande/Desktop')
write.csv(stocks_close_df, file = 'C:/Users/dande/Desktop/stocks.csv')

for (column in colnames(stocks_close_df)) {
  new_name <- paste(str_remove(column, ".Close"), "_r", sep="")
  stocks_close_df[,new_name] <- ROC(stocks_close_df[,column])
}

stocks$msft_r <- ROC(stocks$MSFT.Close)
stocks$aapl_r <- ROC(stocks$AAPL.Close)

setwd("...")
write.zoo(stocks, file = "stocks.csv", sep=",")
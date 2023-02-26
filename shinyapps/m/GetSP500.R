# R Code for S&P500 Stock Analysis
# Download data


# Prerequisites ----------------------------------------------------------------
library(quantmod)   # get stock prices; useful stock analysis functions
library(xts)        # working with extensible time series
library(rvest)      # web scraping
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(stringr)    # working with strings
library(forcats)    # working with factors
library(lubridate)  # working with dates in tibbles / data frames
library(ggridges)   # Ridge plots
library(ggrepel)    # label data points

# Web Scraping: Get the List of S&P500 Stocks ----------------------------------

# Web-scrape S&P500 stock list
sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_node("table.wikitable") %>%
    html_table() %>%
    select(`Symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
    rename(Sector = `GICS Sector`, Industry = `GICS Sub Industry`) %>%
    mutate(Symbol = gsub("\\.","-", Symbol)) %>%  #BRK.B is coded as BRK-B
    as_tibble()

# Format names
names(sp500) <- sp500 %>%
    names() %>%
    str_to_lower() %>%
    make.names()

#sp500 <- sp500[1:10,]

# Creating Functions to Map ----------------------------------------------------
get_stock_prices <- function(ticker, return_format = "tibble", ...) {
    stock_prices <- NULL
    # Get stock prices
    stock_prices_xts <- getSymbols(Symbols = ticker, auto.assign = FALSE, ...) 
    
    # Rename
    names(stock_prices_xts) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    # Return in xts format if tibble is not specified
    if (return_format == "tibble") {
        stock_prices <- stock_prices_xts %>%
            as_tibble() %>%
            drop_na() %>%
            mutate(Date = ymd(index(stock_prices_xts))) # index returns the date from xts object
        
    } else {
        stock_prices <- stock_prices_xts
    }
    Sys.sleep(2)
    stock_prices 
}

toXTS <- function (x){
    # Convert tibble to xts
    if (!is.xts(x)) {
        x <- xts(subset(x, select = -c(Date)), order.by = x$Date)
    }
    x
}

get_sma <- function(x, n=10) {
    x <- toXTS(x)
    # Get stock prices
    SMA10 <- SMA(Cl(x), n = n)
    SMA10[length(SMA10)]
}

get_RSI <- function(x, n=14) {
    x <- toXTS(x)
    # Calculate RSI
    tem <- RSI(Cl(x), n = 14)
    tail(tem, 1)
}

get_MACD <- function(x, ...) {
    x <- toXTS(x)
    # Calculate RSI
    tem <- MACD(Cl(x), nFast = 12, nSlow = 26, nSig = 9)
    tail(tem, 1)
}

# days since reversal
get_MACD_days <- function(x, ...) {
    x <- toXTS(x)
    # Calculate RSI
    tem <- MACD(Cl(x), nFast = 12, nSlow = 26, nSig = 9)
    
    diff = tem$macd - tem$signal
    signs <- rev( as.numeric( sign(diff) ) ) # take the sign and then reverse the sequence
    signs <- !duplicated( signs) 
    signs[1] <- FALSE # assign the first to FALSE
    #match(TRUE, signs) -1  # days since reversal of trend
    
    tail(match(TRUE, signs) -1  )
}

get_ADX <- function(x, ...) {
    x <- toXTS(x)
    # Calculate RSI
    tem2 <- ADX(x, n = 14)
    tem <- tail(tem2, 1)
    # names(tem) <- colnames(tem2)
}

get_daily_returns <- function(x, ...) {
    
    x <- toXTS(x[,c("Close","Date")])
    tail( dailyReturn(x), 30)
}



# Mapping the Functions --------------------------------------------------------
from <- today() - 300
to   <- today()
sp500 <- sp500 %>%
    mutate(
        stock.prices = map(symbol,
                           function(.x) get_stock_prices(.x,
                                                         return_format = "tibble",
                                                         from = from,
                                                         to   = to)
        )
    )


# calculate technical indicators -------------------------------------------
sp500 <- sp500 %>%
    mutate(
        SMA3 = map(stock.prices, function(.x) get_sma(.x, n=3 )   ),
        SMA10 = map(stock.prices, function(.x) get_sma(.x, n=10 )   ),
        SMA20 = map(stock.prices, function(.x) get_sma(.x, n=20 )   ),
        SMA50 = map(stock.prices, function(.x) get_sma(.x, n=50 )   ),
        #SMA100 = map(stock.prices, function(.x) get_sma(.x, n=100 )   ),
        RSI = map(stock.prices, function(.x) get_RSI(.x)   ),    
        MACD = map(stock.prices, function(.x) get_MACD(.x)   ),
        days = map(stock.prices, function(.x) get_MACD_days(.x) ), # days in current trend
        ADX = map(stock.prices, function(.x) get_ADX(.x)
        #,dailyReturn = map(stock.prices, function(.x) get_daily_returns(.x )   ) 
        )
    )



# Split columns as some indicators  return multiple values-----------------------------
sp500$DIp = 0
for( i in 1:nrow(sp500))
    sp500$DIp[i] <- as.vector(sp500$ADX[[i]])[1]
sp500$DIn = 0
for( i in 1:nrow(sp500))
    sp500$DIn[i] <- as.vector(sp500$ADX[[i]])[2]
sp500$DX = 0
for( i in 1:nrow(sp500))
    sp500$DX[i] <- as.vector(sp500$ADX[[i]])[3]

for( i in 1:nrow(sp500))
    sp500$ADX[i] <- as.vector(sp500$ADX[[i]])[4]



sp500$macd = 0
for( i in 1:nrow(sp500))
    sp500$macd[i] <- as.vector(sp500$MACD[[i]])[1]

sp500$macd.signal = 0
for( i in 1:nrow(sp500))
    sp500$macd.signal[i] <- as.vector(sp500$MACD[[i]])[2]

sp500$macd.diff = sp500$macd - sp500$macd.signal


sp500 <- sp500 %>%
    select( -c(MACD)) %>%
    as.data.frame() %>%
    mutate(RSI = as.numeric(RSI)) %>%
    mutate(macd = as.numeric(macd)) %>%
    mutate(macd.diff = as.numeric(macd.diff)) %>%
    mutate(ADX = as.numeric(ADX)) %>%
    mutate(days = as.numeric(days))

save.image(file = "sp500_data.RData")


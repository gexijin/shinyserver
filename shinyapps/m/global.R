load("/srv/data/sp500_data.RData") # linux
#load("sp500_data.RData") # windows
# Plot the stock price
splot <- function (ticker, days) { 
    data <- getSymbols(ticker, from=today()- days, to=today(), auto.assign = FALSE)  
    chartSeries(data,
                theme=chartTheme('white'), 
                name= ticker) 
    plot( addSMA(n=3,on=1,col = "blue") ) # plot is needed, otherwise the line is not there
    plot( addSMA(n=10,on=1,col = "red") )
    plot( addRSI(n=14,maType="EMA") )
    plot( addMACD(fast = 3, slow = 10, signal = 5) )
}

toXTS <- function (x){
    # Convert tibble to xts
    x <- x %>%
        drop_na() # drop rows with NA. This occurs in some tickers such as HWM
    
    if (!is.xts(x)) {
        x <- xts(subset(x, select = -c(Date)), order.by = x$Date)
    }
    x
}

Symbols <- c("^GSPC", "QQQ","VIXY",
             "SPG", "KSS", # retail
             "DAL", "UAL", "BA",# Airlines
             "MRO", "PSX", # Oil
             "MGM", "MAR",  #hotel
             "CCL", "NCLH", # Cruise
             "COTY", "DIS",
             "C", "WFC", "HRB", "SCHW",
             "FB","AMZN", "MSFT", "GOOG", "KC",
             "DOCU","ZM"
) # 
names(Symbols) <- Symbols
names(Symbols)[1] <- "S&P 500" # ^GSPC at Yahoo

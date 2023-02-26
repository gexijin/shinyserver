# For testing the forecasting using seasonal component. 
library(tidyverse)
library(forecast)

d2 <- read.csv("https://api.covidtracking.com/v1/us/daily.csv" ) %>%
  mutate(positiveRate = positiveIncrease / (1 + totalTestResultsIncrease) *100 ) %>%
  select(date, positiveIncrease, hospitalizedCurrently, deathIncrease,
         positiveRate, totalTestResultsIncrease, inIcuCurrently ) %>%
  rename(cases = positiveIncrease ) %>%
  rename(hospitalized = hospitalizedCurrently ) %>%
  rename(death = deathIncrease ) %>%
  rename(nTests = totalTestResultsIncrease ) %>%
  rename(ICU = inIcuCurrently ) %>%
  replace_na(list(cases = 0, hospitalized = 0, death = 0, ICU = 0)) %>%
  mutate(date = as.Date(as.character(date), tryFormats = "%Y%m%d")) %>%
  arrange(date) %>%     #sort
  filter(date >= as.Date("2020-03-1") ) %>%  # remove earlier 
  filter(date <= as.Date("2020-12-25") )   # only keep data up to 12-25 

d2 <- d2 %>%               
  rename(time = date) %>%
  arrange(time) 


npMax <- 70  # only use last 5 weeks of data for forecast
np <- nrow(d2) # number of time points
if(np > npMax)
  d2 <- d2[(np-npMax+1):np,]

# convert to time series using frequency of 7, weekly
confirm <- ts(d2$death, frequency=7  )

# forecast based on  https://otexts.com/fpp2/forecasting-decomposition.html

forecasted <- stlf(confirm, 
                   robust = TRUE, 
                   h = 14, 
                   level = c(90),
                   #method = "naive"
                   #lambda="auto",
                   method='ets',
                   etsmodel = "AAN"
                   #etsmodel = "MAM"    
                   #biasadj=TRUE,
                   #damped=TRUE
)

# This results looks odd, heavily skewed by most recent data points
autoplot(forecasted)


# without the seasonal component
library(lubridate)
confirm <- ts(d2$death, # percent change
              start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )

forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), h=14 )

# the results are reasonable
autoplot(forecasted)


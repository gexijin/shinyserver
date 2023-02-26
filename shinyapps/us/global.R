# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 
options(warn=-1)

library(tsibble)
library(fabletools)  # plot time series
library(dplyr)
library(tidyr)
library(lubridate)
library(slider) # for moving average
library(plotly)
library(usmap)   # map ploting
library(ggplot2)
library(corrplot) # for correlation plot
library(feather) # for faster loading of data frames
library(ggrepel) # text annotation

#library(ComplexHeatmap) # for heat map
#library(circlize)   # for heatmap

library(forecast) # time series

npMax <- 70  # only use last 5 weeks of data for forecast
plotWidth = 800
nPoints = 7
#beginning = as.Date("2020-03-1")
#####################################
#  Read in Google Trend data
####################################
#setwd( "C:/Users/Xijin.Ge/Google Drive/research/covid-19/src/GT")
#------------Interest over time

Trend <- read_feather("../../data/us/Trend.feather")


#------------Interest by region
statesGeo <- read_feather("../../data/us/statesGeo.feather")


##################################################
#  Read Covid data from the covid tracking project
##################################################

### US. Data from the COVID Tracking project------------------------------------------------

CTPraw <- read_feather("../../data/us/CTPraw.feather")

CTP <- read_feather("../../data/us/CTP.feather")

### State-level Data from the COVID Tracking project---------------------------------------

CTPstateRaw <- read_feather("../../data/us/CTPstateRaw.feather")    

CTPstate <- read_feather("../../data/us/CTPstate.feather") 
#write.csv(CTPstate, "tem.csv")

###############################################
#  Google Mobility data
##############################################

mobility <- read_feather("../../data/us/mobility.feather") 

# column names
Mobility <- c("Transit Station", "Workplaces", "Retail & Recreation", "Grocery & Phamarcy")

Disease <- c("Confirmed cases", "Hospitalization",   
             "Deaths", "%Positive Tests", "N. of Tests","In ICU" )

keywords <- c("\"COVID symptoms\"", 
              "\"COVID testing\"", 
              "\"Loss of smell\"", 
              "\"Loss of taste\"", 
              "\"COVID\"", 
              "\"iPhone\"")

# !!! Note that the same keywords and in the same 
# order are used in inerest over time data and the geographical data. !!!

# prepare list of state names
stateNames = c("US", state.abb)
names(stateNames) = c("US", state.name)

mapData <- read_feather("../../data/us/mapData.feather") 
#write.csv(mapData, "mapData.csv")

nameKey = list(   
    word1 = keywords[1],
    word2 = keywords[2],
    word3 = keywords[3],
    word4 = keywords[4],
    word5 = keywords[5],
    word6 = keywords[6],
    cases = Disease[1],    
    hospitalized = Disease[2],
    death = Disease[3],
    positiveRate = Disease[4],
    nTests = Disease[5],
    ICU = Disease[6],
    transit = Mobility[1],
    workplaces = Mobility[2],  
    retail = Mobility[3],
    grocery = Mobility[4]

)


# missing data imput using the mean of n neighboring data points on both sides
# if n = 1, then two neighbors, if n=2 then 2 neighbors on both sides
meanImput <- function (x, n = 2) { 
    ix <- is.na(x)
    x2 <- x
    for( ixx in which(ix)) {
        start <- ixx-n;
        if(start < 1) 
            start <- 1;
        end <- ixx + n;
        if(start > length(x)) 
            start <- length(x);  
        x2[ixx] <- mean( x[ start:end], na.rm = T  ) }
    return( x2 )
}

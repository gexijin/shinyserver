install.packages(c("forcats","ggrepel","forecast","plotly","shinyBS","lubridate"))
install.packages("remotes")
remotes::install_github("GuangchuangYu/nCov2019")  # main data package
remotes::install_github("GuangchuangYu/chinamap")   #Chinese map
install.packages(c("sp","mapproj","maps","sf"))
remotes::install_github("pzhaonet/pinyin")
install.packages("shadowtext")
install.packages("conflicted")
# Get version 3.3.0 for ggplot2
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
remotes::install_github("RamiKrispin/coronavirus")
install.packages("broom")
remotes::install_github("RamiKrispin/covid19Italy")
install.packages("usmap")
install.packages("ggridges") 

# for USD server
USDlist = c("pacman", "foreign", "questionr", "sessioninfo", "plyr", "stringi", 
       "psych", "ggthemes", "extrafont", "scales", "waffle",
       "tigris",  "tidycensus", "viridis", "qualtRics",  "janitor", "rgdal",
       "RColorBrewer", "googlesheets4")
#install.packages(USDlist)

# If you do not have BiocManager installed:
#install.packages('BiocManager')
# Then, if sars2pack is not already installed:
#BiocManager::install('seandavi/sars2pack')

install.packages("tsibble")
install.packages("fabletools")  # plot time series
install.packages("tidyr")
install.packages("slider") # for moving average
install.packages("corrplot") # for correlation plot
#install.packages("circlize")   # for heatmap

install.packages("feather")  # for faster loading of data frames.

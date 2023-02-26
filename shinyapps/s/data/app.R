# This is a Shiny app for showing COVID-19 survey data in SD

library(pacman)
p_load(knitr, foreign, tidyverse, questionr, sessioninfo, plyr, stringi, 
       dplyr, psych, ggplot2, ggthemes, extrafont, plyr, scales, reshape2, waffle,
       tigris, lubridate, sf, tidycensus, viridis, qualtRics, maps, janitor, rgdal,
       RColorBrewer, googlesheets4)

#load API key for US Census
census_api_key("181a127c4ed1cc690b609abeb8323fbf8d0ef52e")

#-------------------------------------------------------------------------
# Prepare data
#-------------------------------------------------------------------------
#find all files with the survey name 
#sort by date and select most recent

#load most recent survey from working dir
data_files <- file.info(Sys.glob("data/South*.csv"))
most_recent <- row.names(data_files)[which.max(data_files[["ctime"]])]
survey_data <- read_survey(most_recent) %>% 
    slice(-1) %>%
    select(-IPAddress)


survey <- survey_data[which(survey_data$Status == 0),]
##Recovering first 5 digits of zip codes in case of zip+ entered.
survey$zip <- substr(survey$Q35, 1, 5)

##converting zip codes to counties based on the 2019 4th quarter file and selcting the countiy with most addresses in the zip code (https://www.huduser.gov/portal/datasets/usps_crosswalk.html#codebook)

zips <- read.csv ("data/ziptable.csv", colClasses=c("zip" = "character"))

zips1 <- zips[,-c(3:6)]
##Changing shannon county code
zips1[zips1$FIPS == 46113, "FIPS"] <- 46102

survey_zip <- merge(survey, zips1, by.x=c("zip"),
                    by.y=c("zip"))

survey_zip <- transform(survey_zip, state = substr(FIPS, 1, 2))
survey_zipSD <- survey_zip[which(survey_zip$state == 46),]


#-------------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------------

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


#-------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

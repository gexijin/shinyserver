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


#-----------------------------------------------------------------------------
#get population estimates for each county
county_pop_acs <- get_acs(variables = "B00001_001", state = "46", geography = "county", geometry = T,)
#get responses for each county and merge with population
county_count <- data.frame(table(survey_zipSD$FIPS))

surv_count_pop <- merge(county_count, county_pop_acs, by.x=c("Var1"),
                        by.y=c("GEOID"))

surv_count_pop$perpop <- surv_count_pop$Freq/surv_count_pop$estimate *1000
surv_count_pop$lowercounty<-tolower(gsub(" County.*$", "" , surv_count_pop$NAME ))

names(surv_count_pop)[names(surv_count_pop) == 'Var1'] <- 'fips'

# align data with map definitions by (partial) matching state,county
# names, which include multiple polygons for some counties

us_states_coords <- map_data("state")
world_coords <- map_data("world")
sd_county_coords <- map_data("county") %>% filter(region == "south dakota")

sd_county_coords[sd_county_coords$subregion == 'shannon', "subregion"] <- 'oglala lakota'

sd_data <- merge(sd_county_coords, surv_count_pop, by.x=c("subregion"),
                 by.y=c("lowercounty"), all.x = TRUE)

sd_data <- sd_data[order(sd_data$order),]

#----------------------------------------------------------------------------------
##Data manipulations for Q24 Trused sources

##First separating out responses
survey_zipSD$Q24R1Ind <- grepl("1", survey_zipSD$Q24)
survey_zipSD$Q24R2Ind <- grepl("2", survey_zipSD$Q24)
survey_zipSD$Q24R4Ind <- grepl("4", survey_zipSD$Q24)
survey_zipSD$Q24R5Ind <- grepl("5", survey_zipSD$Q24)
survey_zipSD$Q24R6Ind <- grepl("6", survey_zipSD$Q24)
survey_zipSD$Q24R7Ind <- grepl("7", survey_zipSD$Q24)
survey_zipSD$Q24R8Ind <- grepl("8", survey_zipSD$Q24)
survey_zipSD$Q24R9Ind <- grepl("9", survey_zipSD$Q24)
survey_zipSD$Q24R10Ind <- grepl("10", survey_zipSD$Q24)
survey_zipSD$Q24R11Ind <- grepl("11", survey_zipSD$Q24)
survey_zipSD$Q24R1IndReal <- survey_zipSD$Q24R1Ind & !(survey_zipSD$Q24R10Ind) & !(survey_zipSD$Q24R11Ind)

#Then counting the total number with responses to get the denominator
temp <- survey_zipSD[which(survey_zipSD$Q24 != ''),]



#-------------------------------------------------------------------------
# User Interface
#-------------------------------------------------------------------------

library(shiny)

plots <- c("concern", "lifeStyle", "isolation")
names(plots) <- c("Concern about COVID-19", "Life style", "Sel-Isolation")



ui <- fluidPage(

    # Application title
    titlePanel("South Dakota Survey on COVID-19"),
    
    h4("This project is led by public health and healthcare professionals in the joint USD-SDSU Master of 
    Public Health program and USD’s School of Medicine and Department of Psychology. Our goal is to help 
    learn how individuals and families are affected by and reacting to this pandemic. Currently the survey has been 
       taken by 4269 individuals. Of those, 3531 provided a valid zip code and 3483 were from South Dakota."),

    fluidPage(
           img(src='MissouriRiver-Survey.jpg', align = "center",width="700", height="300"),
           br(),br(), # space
           h4("The map below shows the responses by county within the state of South Dakota for those who provided zip code information."),
           plotOutput("map"),
           selectInput("selectPlot", "Select plot", choices = plots, selected = plots[1]),
           
           conditionalPanel("input.selectPlot == 'concern'",
                            plotOutput("concern"),
                            h4("This shows ...")
           ),
           conditionalPanel("input.selectPlot == 'lifeStyle'",
                            plotOutput("lifeStyle")

           ),
           conditionalPanel("input.selectPlot == 'isolation'",
                            plotOutput("isolation")

           )
           

    )
)


#-------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
        #map SD counties. Fill with survey responses
        plot_responses_sd <- sd_data %>% 
            ungroup() %>% 
            ggplot() +
            geom_polygon(aes(x = long, y = lat, group = group, fill = perpop), 
                         color = 'grey20') +
            scale_fill_distiller(direction = 1, na.value = "white") +
            coord_map() +
            theme_void() +
            
            NULL
        
        plot_responses_sd + labs(fill = "Survey responses\nper 1000 people")
    })   
    
    output$concern <- renderPlot({
        ##Q2 concern about COVID-19
        
        
        y = plyr::count(survey_zipSD, 'Q2')
        y = y[1:5,]
        y$perc <- (y$freq/sum(y$freq))*100
        y$perc1 <- format(round (y$perc, 2), nsmall = 2)
        y$Q2 <-factor(y$Q2, 
                      levels = c("1", "2", "3", "4", "5"))
        
        p <-ggplot(y, aes(Q2, perc)) + geom_bar(stat = "identity", fill="#2171B5",
                                                color = "white", width=0.5 ) +
            xlab("Level of Concern about COVID-19") + ylab("Percent") + 
            scale_x_discrete(labels = c("1" = "Not at all concerned","2" = "A little concerned", 
                                        "3" = "Moderately concerned","4" = "Very concerned",
                                        "5" = "Extremely concerned")) +
            scale_y_continuous(                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
                                                    label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"), 
                                                    limits = c(0,80))
        p+theme_classic()+ geom_text(aes(label=perc1), vjust = -0.15)
        
        
        p
    })  
    
    
    output$lifeStyle <- renderPlot({
        ##Q3 Changes to your lifestyle
        
        y = plyr::count(survey_zipSD, 'Q3')
        y = y[1:4,]
        y$perc <- (y$freq/sum(y$freq))*100
        y$perc1 <- format(round (y$perc, 2), nsmall = 2)
        y$Q3 <-factor(y$Q3, 
                      levels = c("1", "2", "3", "4"))
        
        p <-ggplot(y, aes(Q3, perc)) + geom_bar(stat = "identity", fill="#2171B5",
                                                color = "white", width=0.5 ) +
            xlab("Changes to Your Lifestyle") + ylab("Percent") + 
            scale_x_discrete(labels = c("1" = "Not at all","2" = "Very little", 
                                        "3" = "Somewhat","4" = "To a great extent")) +
            scale_y_continuous(                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
                                                    label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"), 
                                                    limits = c(0,80))
        p+theme_classic()+ geom_text(aes(label=perc1), vjust = -0.15)
        
        p
    })   
    
    output$isolation <- renderPlot({
        ##Q5 Self-isolation
        
        y = plyr::count(survey_zipSD, 'Q5')
        y = y[1:4,]
        y$perc <- (y$freq/sum(y$freq))*100
        y$perc1 <- format(round (y$perc, 2), nsmall = 2)
        y$Q5 <-factor(y$Q5, 
                      levels = c("1", "2", "3", "4"))
        
        p <-ggplot(y, aes(Q5, perc)) + geom_bar(stat = "identity", fill="#2171B5",
                                                color = "white", width=0.5 ) +
            xlab("Self-Isolation") + ylab("Percent") + 
            scale_x_discrete(labels = c("1" = "All of the time","2" = "Most of the time", 
                                        "3" = "Some of the time","4" = "None of the time"),limits = rev(levels(y$Q5))) +
            scale_y_continuous(                     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
                                                    label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"), 
                                                    limits = c(0,80))
        p+theme_classic()+ geom_text(aes(label=perc1), vjust = -0.15)
        p
    })  
}

# Run the application 
shinyApp(ui = ui, server = server)

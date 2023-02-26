# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 


#################################################
#  UI
#################################################
library(plotly)
library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("COVID-19, community mobility and internet searches"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectState", NULL,
                        choices = stateNames,
                        selected = stateNames[1]),
            checkboxGroupInput("selectWords", 
                               h5("Frequences of Google searches:"), 
                               choices = keywords,
                               selected = keywords[1]),
            checkboxGroupInput("selectSeries", 
                               h5("COVID-19 Statistics:"), 
                               choices = Disease,
                               selected = Disease[1]),            

            checkboxGroupInput("selectMobility", 
                               h5("Google Mobility relative to 2/2020"), 
                               choices = Mobility,
                               selected = NULL),

            h6("*Select two to examine correlation*"),
            
            sliderInput("selectMA", h5("Moving average (days):"),
                        min = 1, max = 14, value = 7),
            p(HTML("<div align=\"right\"> <A HREF=\"javascript:history.go(0)\">Reset</A></div>" ))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Trends",
                    plotlyOutput("US_GT_Plot"), 
                    br(),br(),
                    textOutput("currentStats")
                   # plotOutput("crossCorrelationPlot")
                ),
                
                tabPanel("Maps"
                         ,h4("Select two metrics to investigte spatial correlation. Please wait....")
                         ,plotlyOutput("stateMap")
                         ,plotlyOutput("stateMap2")
                         ,plotOutput("stateScatter")
                ),
                
                tabPanel("Correlations"
                         ,plotOutput("corrMatrix")
                ),
                
                #tabPanel("Heatmap", 
                #         plotOutput("heatmap")
                #         ),
                
                tabPanel("Projections"
                         ,h4("Select one  COVID-19 statistics.")
                         ,sliderInput("daysForcasted", h5( paste("Days to project from ", max(CTP$date)) )  ,
                                      min = 1, max = 21,
                                      value = 14)
                         ,tableOutput("predictedTable")
                         ,br()
                         ,plotOutput("stateProjectionNoSeasonal")  
                         ,br()
                         ,plotOutput("stateProjection")


                ),
                
                tabPanel("About"
                         ,h5("Data sources:",
                             "Covid-19 statistis is from", a("the Covid tracking project.",href="https://covidtracking.com/"), 
                             "Google search frequencies are downloaded from", a("Google Trends", href="https://trends.google.com/trends/?geo=US"), 
                             " using the", a("pytrends API.",href="https://pypi.org/project/pytrends/"), 
                             "Mobility data is retrieved from",
                             a("Google Community Mobility Report,",href="https://www.google.com/covid19/mobility/"), 
                             "which is aggregated, anonymized location data from smart phones."
                         )    
                         ,h5("For feedbacks or suggestions  please contact me via "
                             ,a("email ",href="mailto:xijin.ge@sdstate.edu?Subject=Coronavirus website" ), "or", 
                             a("Twitter.", href="https://twitter.com/StevenXGe"),
                             "My research interests are genomics, bioinformatics, and data science ",
                             a("(lab homepage).", href="http://ge-lab.org/"), 
                             "Source code on ", a("GitHub,",href="https://github.com/gexijin/covid"), 
                             "where you can also report bugs or other issues.",
                             "Not an epidemiologist or statistician. Accuracy not guaranteed.")                         
                         
                ) 
                
                
            )
        )
    )
    
    ,tags$head(includeScript("ga.js")) # tracking usage with Google analytics  
)


# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020

library(shiny)


#library(shinyBS,verbose=FALSE) # for popup figures

ui <- fluidPage(
    #titlePanel("GWW"),
    tabsetPanel(
        tabPanel("Market"
                 ,plotOutput("rsiPlot")
                 ,br(),br()
                 ,plotOutput("macdPlot")
                 ,br(),br()
                 ,plotOutput("adxPlot")
                 ,br(),br()
                 
        ) #tab2 --------------------------------------------------
        ,tabPanel("Screen"
                  ,fluidRow(
                      column(3, selectInput("sector", "Sector", c("All"))
                             ),
                      column(3, sliderInput("macd.diff",
                                             "MACD-signal",
                                             min = -4,
                                             max = 4,
                                             value = c(-4,4)) 
                      ),                      
                      column(3, sliderInput("RSI",
                                             "RSI",
                                             min = -4,
                                             max = 4,
                                             value = c(-4,4)) 
                      ),                      
                      column(3, sliderInput("ADX",
                                             "ADX",
                                             min = 0,
                                             max = 100,
                                             value = c(0,100)) 
                      )                     
                  )
                 ,plotOutput("sectorScatterPlot")
                 ,selectInput("topStocks", "Ticker", NULL) 

                 #,DT::dataTableOutput('selectedStocks')

                 ,plotOutput("stockPlot")
                 #,plotOutput("stockPlot2")
                 
        ) #tab2 --------------------------------------------------
        
        ,tabPanel("Watch List",
                  sidebarPanel(
                      sliderInput("days",
                                  "Number of days:",
                                  min = 52,
                                  max = 365,
                                  value = 90)
                      ,fluidRow(
                          column(8,  textInput("ticker", NULL, 
                                               value = "ticker")  ),
                          column(4,  actionButton("Next", "Next") )
                      )
                      ,radioButtons("radio", NULL,
                                   choices = Symbols)  
                      
                      )
                  
                  ,mainPanel(
                      plotOutput("stockPlot2")
                  )
                  
        
                  

                  
        ) 
        
        ,tabPanel("About Momenta"
                  ,h5("No guarantee of accuracy. Not responsible for any errors.")
                  ,h5("7/5/20 V. 0.1 It's running")

        )
    )
    #,tags$head(includeScript("ga.js")) # tracking usage with Google analytics      
)
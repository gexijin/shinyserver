# server logic for WGG stock screener
# xijin Ge

library(shiny)
library(ggridges)   # Ridge plots
library(ggrepel)    # label data points
library(tidyverse)  # ggplot2, purrr, dplyr, tidyr, readr, tibble
library(quantmod)   # get stock prices; useful stock analysis functions
library(lubridate)  # working with dates in tibbles / data frames

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    observe({
      isolate({ 
          minD <- floor( min(sp500$macd.diff) *10) / 10
          maxD <- ceiling( max(sp500$macd.diff) *10) / 10
          updateSliderInput(session, "macd.diff", min = minD, max = maxD, value = c(minD, maxD))
          
          minD <- floor( min(sp500$RSI) ) 
          maxD <- ceiling( max(sp500$RSI) ) 
          updateSliderInput(session, "RSI", min = minD, max = maxD, value = c(minD, maxD))          

         updateSelectInput(session, "sector", choices = c(unique(sp500$sector), "All") )          

         updateSliderInput(session, "ADX", 
                            min = floor( min(sp500$ADX)), 
                            max = ceiling(max(sp500$ADX)), 
                            value = c(min(sp500$ADX), ceiling(max(sp500$ADX)) ) )           
       })
    })
    
    observe({
        input$sector
        input$macd.diff
        selected <- selected() %>%
            arrange(desc(macd.diff)) %>%
            pull(symbol)
        
        updateSelectInput(session, "topStocks", choices = selected )

    })

    output$rsiPlot <- renderPlot({
        # plot RSI by sector
        ggplot(sp500, aes(RSI, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            geom_vline(xintercept = 30, linetype="dashed", color = "green", size = 1) +
            geom_vline(xintercept = 70, linetype="dashed", color = "red", size = 1) +
            theme(axis.title.y=element_blank() ) +
            xlab("RSI (>70 overbought; <30 oversold)") +
            xlim(c(0,100))
    })
    
    output$macdPlot <- renderPlot({
        # plot MACD - signal by sector
        ggplot(sp500, aes(macd.diff, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            theme(axis.title.y=element_blank() ) +
            geom_vline(xintercept = 0, linetype="dashed", color = "red", size = 1) + 
            xlab("MACD - Signal (>0 trending up)") +
            xlim(c(-4,2))
    })
    
    output$adxPlot <- renderPlot({
        # plot MACD - signal by sector
        ggplot(sp500, aes(ADX, sector, fill = sector)) + 
            geom_density_ridges() +
            theme(legend.position = "none") +
            theme(axis.title.y=element_blank() ) +
            xlab("ADX indicator (>25 strong trend)") +
            geom_vline(xintercept = 25, linetype="dashed", color = "red", size = 1) +  #not work
            xlim(c(0,40))
    })  
    
    
# Screening --------------------------------------------------------------------
    output$selectedStocks <- DT::renderDataTable({
        # does not show up
        if( is.null( input$selectedSpecies)) return(NULL) 
        
        selected <- sp500 %>%
            filter(sector == input$sector) %>%
            arrange(desc(macd.diff))
        
        selected
        
    }, selection = 'single', options = list(pageLength = 5 ) # only 5 rows shown
    )
    
    selected <- reactive({
        #screening stocks
        if(input$sector != "All") { 
            tem <- sp500 %>% 
                filter(sector == input$sector) 
        } else {
            tem <- sp500
        }
        tem %>%
            filter(macd.diff > input$macd.diff[1] & macd.diff < input$macd.diff[2]) %>%
            filter(ADX > input$ADX[1] & ADX < input$ADX[2]) %>%
            filter(RSI > input$RSI[1] & RSI < input$RSI[2])
            
    })
    
    output$sectorScatterPlot <- renderPlot({
        # plot MACD - signal vs. RSI for sectors
        selected() %>%
            ggplot(aes(macd.diff, RSI, label = symbol)) + 
            geom_point() +
            geom_text_repel() +
            theme(legend.position = "none") +
            xlab("MACD - Signal")  + 
            geom_vline(xintercept = 0, linetype="dashed", color = "red", size = 1  )+
            xlim(c(-2,2)) +
            ylim(c(min(sp500$RSI), max(sp500$RSI)))
    })      
 
 
    
    output$stockPlot <- renderPlot({
        # using downloaded data
        input$sector
        if(is.null(input$topStocks))
            return(NULL)
      
        # row ID of selected
        ix <- match(input$topStocks, sp500$symbol)
        
        data <- toXTS( tail( sp500$stock.prices[[ix]], 100 ) ) #100 days
        chartSeries(data,
                    theme=chartTheme('white'), 
                    name= sp500$symbol[ix]) 
        plot( addSMA(n=3,on=1,col = "blue") ) # plot is needed, otherwise the line is not there
        plot( addSMA(n=10,on=1,col = "red") )
        plot( addRSI(n=14,maType="EMA") )
        plot( addMACD(fast = 12, slow = 26, signal = 9) )
        
    }, height = 800)
    
    
    observe({
        # use the reminder so that if Next keeps increasing passed 24, it returns to 1
        ix <- input$Next %% length(Symbols) + 1
        updateRadioButtons(session, "radio",
                           label = NULL,
                           choices = Symbols,
                           selected = Symbols[ ix ]
        )
    })
    
    output$stockPlot2 <- renderPlot({
        # plot stock price with data from Yahoo
        
        ticker <- input$ticker
        
        if(input$ticker == "ticker")
            ticker <- input$radio 
        
        splot( ticker, input$days)
    }, height = 800)
    
    
})

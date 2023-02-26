# For showing covid and google search
# this program investigates the correlation between google search
# and covid cases
# Based on https://ramikrispin.github.io/halloween-time-series-workshop/index.html
# By Xijin Ge   Xijin.Ge@sdstate.edu     http://ge-lab.org/ 


###############################################
#  server
###############################################

function(input, output, session) {

    mergedData <- reactive({
        # This function prepares data based on user selection
        # Returns a long form time-series data
        
        if(is.null(input$selectState)) 
            return(NULL)        

        ##########################################
        # merge Google trends, mobility, with cases
        ##########################################
        
        # filter Google Trends data based on selected state
        TrendRegion <- Trend %>%
            filter(Region == input$selectState) %>%  # "US", "AL"
            select(-Region)

        # filter Google mobility data based on selected state
        mobilityRegion <- mobility %>%
            filter( Region == input$selectState) %>%   
            select(-Region)
        
        # filter covid data by state
        if(input$selectState == 'US') {
            CTPRegion <- CTP 
        } else {
            CTPRegion <- CTPstate %>%
                filter(state == input$selectState) %>%
                select(-state)
        }
        
        #Merge mobility, search and infection data.
        merged <- full_join(TrendRegion, CTPRegion, by="date")  %>%
            full_join(mobilityRegion, by = 'date') %>%
            select(c(date, cases, hospitalized, death, positiveRate, nTests, ICU,
                     word1, word2, word3, word4, word5, word6, 
                     workplaces, transit, grocery, retail)) %>%
            gather("key", "value", -date) %>%     # convert to long form
            as_tsibble(key = key, index = date)   # convert to tsibble object

        # calculate moving averages     # https://davisvaughan.github.io/slider/
        merged3 <- merged %>% 
            group_by_key() %>% 
            mutate(Daily_MA = slide_dbl(value, 
                                        ~mean(.x, na.rm = TRUE),
                                        .before = input$selectMA - 1, # 7 day moving average
                                        .after = 0
            )) %>%
            mutate( key = recode(key, 
                                 word1 = keywords[1],
                                 word2 = keywords[2],
                                 word3 = keywords[3],
                                 word4 = keywords[4],
                                 word5 = keywords[5],
                                 word6 = keywords[6],
                                 hospitalized = Disease[2],
                                 cases = Disease[1],
                                 death = Disease[3],
                                 positiveRate = Disease[4],
                                 nTests = Disease[5],
                                 ICU = Disease[6],
                                 transit = Mobility[1],
                                 retail = Mobility[2],
                                 grocery = Mobility[3],
                                 workplaces = Mobility[4]
                                 ) ) %>%
          arrange(date, key)

        
        #----------------------------------------------------
        # add raw counts as hover text
        # almost exactly as above but use raw counts for Covid data
        
        # filter covid data by state
        if(input$selectState == 'US') {
          CTPRegionRaw <- CTPraw 
        } else {
          CTPRegionRaw <- CTPstateRaw %>%
            filter(state == input$selectState) %>%
            select(-state)
        }
        
        #Merge mobility, search and infection data.
        mergedRaw <- full_join(TrendRegion, CTPRegionRaw, by="date")  %>%
          full_join(mobilityRegion, by = 'date') %>%
          select(c(date, cases, hospitalized, death, positiveRate, nTests, ICU,
                   word1, word2, word3, word4, word5, word6, 
                   workplaces, transit, grocery, retail)) %>%
          gather("key", "value", -date) %>%     # convert to long form
          as_tsibble(key = key, index = date)   # convert to tsibble object
        
        # calculate moving averages     # https://davisvaughan.github.io/slider/
        mergedRaw <- mergedRaw %>% 
          group_by_key() %>% 
          mutate(Daily_MA_raw = slide_dbl(value, 
                                      ~mean(.x, na.rm = TRUE),
                                      .before = input$selectMA - 1, # 7 day moving average
                                      .after = 0
          )) %>%
          mutate( key = recode(key, 
                               word1 = keywords[1],
                               word2 = keywords[2],
                               word3 = keywords[3],
                               word4 = keywords[4],
                               word5 = keywords[5],
                               word6 = keywords[6],
                               hospitalized = Disease[1],
                               cases = Disease[2],
                               death = Disease[3],
                               positiveRate = Disease[4],
                               nTests = Disease[5],
                               ICU = Disease[6],
                               transit = Mobility[1],
                               retail = Mobility[2],
                               grocery = Mobility[3],
                               workplaces = Mobility[4]
          ) ) %>%
          arrange(date, key)
        
        
        Raw <- format(round(mergedRaw$Daily_MA_raw,0), nsmall=0, big.mark=",")   #1,323,231
        merged3 <- cbind(merged3, Raw)
        colnames(merged3)[5] <- "Raw"
        #write.csv(merged3, "Compiled_data.csv", row.names = FALSE)
        return(merged3)
        # Data looks like
        #date	key	value	Daily_MA	Raw
        #3/1/2020	%Positive Tests	21.81818182	21.81818182	22
        #3/1/2020	Confirmed cases	0	0	0
        #3/1/2020	Deaths	0.087006961	0.087006961	3
        
        
    })
    
    
    
    
    output$US_GT_Plot <- renderPlotly({
        # The changes of mobility, search, and infection over time for a region
        # using plot_ly. 
        # Based on https://ramikrispin.github.io/halloween-time-series-workshop/02-plotting-ts-objects/plotting_ts_objects.html
        if(is.null(mergedData()))
            return(NULL)

        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)

        if(length(selected ) == 0) 
            return(NULL) else { 
                selectedData <- mergedData() %>%
                    filter(key %in% selected) 


                if(nrow(selectedData) < 1) { # if there is no data
                    return(NULL)
                } else { 
                  
                  # removes y axis and labels
                  ay <- list(
                    title = "",
                    zeroline = TRUE,
                    showline = FALSE,
                    showticklabels = FALSE,
                    showgrid = FALSE
                  )
                  
                  ax <- list(
                    title = "",
                    zeroline = FALSE,
                    showline = FALSE,
                    showticklabels = TRUE,
                    showgrid = FALSE
                  )
                
                    p <- plot_ly()
                    
                    for(i in selected ){
                        
                        df <- selectedData %>% 
                            filter(key == i) 
                        # assign values to NA if it is zero. Mostly for ICU
                       # df$Daily_MA[ df$Daily_MA == 0 ] <- NA
                        
                        p <- p %>%
                            add_lines(x = as.Date(df$date),
                                      y = df$Daily_MA,
                                      text = df$Raw,  # show raw numbers on hover
                                      type = "scatter",
                                      mode = "lines",
                                      name = i,
                                      hovertemplate = "%{text}<br>%{x}")
                    }
                }
                
                p %>% layout(legend = list(orientation = "h"), 
                             xaxis = ax, yaxis = ay) 
                
            }
        
    })
    
    
    
    output$crossCorrelationPlot <- renderPlot({
        # when two variables are chosen return a ccf plot
        if(is.null(mergedData()))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) != 2) 
            return(NULL) else { 
                
                selectedData <- mergedData() %>%
                    filter(key %in% selected)   
                
                if(nrow(selectedData) < 1) { # if there is no data
                    return(NULL)
                } else {             
                
                    var1 <- selectedData %>%
                        filter(key == selected[1]) %>%
                        pull(value)
                    var2 <- selectedData %>%
                        filter(key == selected[2]) %>%
                        pull(value)
                    
                    # compute cross correlation
                    # https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-correlation-within-and-among-time-series.html
                    ccf(var1, var2, 
                        lag.max = 21,
                        ylab = "Cross-correlation",
                        main = paste("Cross-correlation of", selected[1], "&", selected[2]),
                        ylim = c(-1, 1)
                        
                        )
    
                    
                    } 
                }
        
    })
    
    output$stateMap <- renderPlotly({
        # plots map using the usmap package
        
         if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) == 0) 
            return(NULL) else { 
                df <- mapData  # a temporary dataframe. Rename the selected column for plotting map
                ix <- grep(selected[1], nameKey) + 4 # 4 columns in the statepop 
                
                colnames(df)[ix] <- "value"
                #write.csv(df,"selected1.csv")
                p <- plot_usmap(data = df, 
                                values = "value", 
                                color = "red") + 
                     scale_fill_continuous( name = selected[1],
                                            low = "white", high = "red",
                                            label = scales::comma ) + 
                     theme(legend.position = "right")
                
                ggplotly(p, tooltip = c("value"))
                
            }
        
    })
    
    
    
    output$stateMap2 <- renderPlotly({
        # plots map using the usmap package
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        if(length(selected ) < 2) 
            return(NULL) else { 
                df <- mapData  # a temporary dataframe. Rename the selected column for plotting map
                ix <- grep(selected[2], nameKey) + 4 # 4 columns in the statepop 
                
                colnames(df)[ix] <- "value"
                #write.csv(df,"selected2.csv")
                p <- plot_usmap(data = df, 
                                values = "value", 
                                color = "red") + 
                    scale_fill_continuous( name = selected[2],
                                           low = "white", high = "red",
                                           label = scales::comma ) + 
                    theme(legend.position = "right")
                
                ggplotly(p, tooltip = c("value"))
                
            }
        
    })   

    
    output$stateScatter <- renderPlot({
        # plots map using the usmap package for the 2nd column
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectSeries, input$selectMobility)
        
        if(length(selected ) < 2) 
            return(NULL) else { 
                
                # COVID numbers are log transformed. 1 is added as there are zeroes. 
                
                #  df <- mapData  %>% # a temporary dataframe. Rename the selected column for plotting map
                #    mutate(hospitalized = log10( 1 + hospitalized)) %>%
                #    mutate(cases = log10(1 + cases)) %>%
                #    mutate(nTests = log10(1 + nTests)) %>%
                #    mutate(death = log10(1 + death)) %>%
                #    mutate(ICU = log10(1 + ICU)) 
                
              
                df <- mapData

                                    
                ix <- grep(selected[1], nameKey) + 4 # 4 columns in the statepop 
                colnames(df)[ix] <- "x"
                
                ix <- grep(selected[2], nameKey) + 4 # 4 columns in the statepop 
                colnames(df)[ix] <- "y"
                
                df <- df %>%
                    select(x, y, abbr) %>%
                    drop_na() 
                
                correlation <- cor.test(df$x, df$y)
                
                if( selected[1] %in% Disease)
                  xlabels <- paste(selected[1], "per 100,000")
                
                if( selected[1] %in% Mobility)
                  xlabels <- paste("Relative mobility in ", selected[1]) 
                
                if( selected[1] %in% keywords)
                  xlabels <- paste("Relative Google search frequencies of ", selected[1], "in the last 7 days")
                
                if( selected[2] %in% Disease)
                  ylabels <- paste(selected[2], "per 100,000")
                
                if( selected[2] %in% Mobility)
                  ylabels <- paste("Relative mobility in ", selected[2], " in the last 7 days") 
                
                if( selected[2] %in% keywords)
                  ylabels <- paste("Relative Google search frequencies of ", selected[2]) 
                
                p <- ggplot(df, aes(x = x, y = y, label = abbr)) +  #size = pop_2015,
                    geom_point(aes(text=sprintf("%s", abbr))) +
                    xlab(xlabels) +
                    ylab(ylabels) +
                    geom_smooth(method='lm') +
                    annotate("text", label = paste0("R=", round(correlation$estimate, 2), 
                                                   " (P =", formatC(correlation$p.value, format = "e", digits = 2), ")"),
                             size = 3,
                             color = "red",
                             x = (min(df$x) + max(df$x) )/2,
                             y = min(df$y)
                             ) +
                  geom_text_repel()
                
                p
                
            }
        
    })

    output$corrMatrix <- renderPlot({
        # Correlation Matrix
        
        if(is.null(mergedData() ))
            return(NULL)
        
        temData <- mapData %>%
            select(-abbr, -fips, -pop_2015, -full, -word6)
        
        colnames(temData)[1:5] <- keywords[1:5]
        
        corrplot(cor(temData), 
                 method="color", 
                 type = "upper", 
                 addCoef.col="black",
                 tl.cex = 1,  # font size for labels
                 number.cex = 1,     # font size for numbers
                 #order = "hclust",   # order using hclust
                 cl.pos = "n", # No color label
                 diag = FALSE)  # no diagonal
        
    }, width = 700, height = 700)    
    
    output$heatmap <- renderPlot({
        # Correlation Matrix
        
        if(is.null(mergedData() ))
            return(NULL)
        
        selected <- c(input$selectWords, input$selectMobility, input$selectSeries)
        
        
        temData <- mapData %>%
            select(-abbr, -fips, -pop_2015, -full, -word6) %>%
            mutate(hospitalized = log10(hospitalized)) %>%
            mutate(cases = log10(cases)) %>%
            mutate(nTests = log10(nTests)) %>%
            mutate(death = log10(death)) %>%
            as.matrix()
        
        row.names(temData) <- mapData$abbr
        
        colnames(temData)[1:5] <- keywords[1:5]
        
        temData <- temData[, selected]
        temData[ is.na(temData)] <- 0
        temData <- temData[ , colSums(is.na(temData)) == 0]
        

        heatmap(temData, scale = "column",   Colv = NA)
        
    }, width = 700, height = 700)  
    
    
    
    
    stateProjectionNoSeasonalData <- reactive ({
      
      selected <- input$selectSeries
      
      if(length( selected ) < 1) 
        return(NULL) else {
          
          d2 <- CTPstateRaw
          
          # entire country
          if(input$selectState == "US") { 
            d2 <- CTPraw 
          } else {
            #selected state
            d2 <- d2 %>%
              filter( state == input$selectState)  
          }
          
          d2 <- d2 %>%               
            rename(time = date) %>%
            arrange(time) 
          
          ix <- grep(selected[1], nameKey)# 4 columns in the statepop
          iy <- grep(names(nameKey)[ix], colnames(d2)) 
          colnames(d2)[iy] <- "selected" 
          
          nRep = sum( d2$selected == d2$selected[2]) 
          if(nRep > 3) 
            d2 <- d2[-(1:(nRep-3)),]
          
          np <- nrow(d2) # number of time points
          if(np > npMax)
            d2 <- d2[(np-npMax+1):np,]
          
          #  if(sum( d2$selected > 5) <10)
          #   return(NULL)
          
          par(mar = c(4, 4, 0, 2))
          # missing data with average of neighbors
          d2$selected<- meanImput(d2$selected, 2)
          
          
          confirm <- ts(d2$selected, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
          
          forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), h=input$daysForcasted )
          
          return(list(forecasted = forecasted, confirm = confirm, d2 = d2))

        }
    } )  
    
    
    
    output$stateProjectionNoSeasonal <- renderPlot ({
      
      
      if(is.null(input$selectSeries ))
        return(NULL)
      selected <- input$selectSeries
      
      par(mar = c(6, 4, 0, 2))
      d2 <- stateProjectionNoSeasonalData()$d2
      confirm <- stateProjectionNoSeasonalData()$confirm
      forecasted <- stateProjectionNoSeasonalData()$forecasted
      
      
      
          
      par(mar = c(6, 4, 2, 2))


          plot(forecasted, xaxt="n", main="Simple trend projection (ets)", 
               ylab = selected[1]
          )
          a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2)  )
          axis(1, at = decimal_date(a), las = 3, labels = format(a, "%b %d"))
        
    } )  
    
    
    projectionData <- reactive ({
        
        if(is.null(input$selectSeries ))
            return(NULL)
        
        selected <- input$selectSeries
        
        if(length( selected ) < 1) 
            return(NULL) else {
                
                d2 <- CTPstateRaw
                
                # entire country
                if(input$selectState == "US") { 
                    d2 <- CTPraw 
                } else {
                    #selected state
                    d2 <- d2 %>%
                        filter( state == input$selectState)  
                }
                
                d2 <- d2 %>%               
                    rename(time = date) %>%
                    arrange(time) 
                
                ix <- grep(selected[1], nameKey)# 4 columns in the statepop
                iy <- grep(names(nameKey)[ix], colnames(d2)) 
                colnames(d2)[iy] <- "selected" 
                
                nRep = sum( d2$selected == d2$selected[2]) 
                if(nRep > 3) 
                    d2 <- d2[-(1:(nRep-3)),]
                
                np <- nrow(d2) # number of time points
                if(np > npMax)
                    d2 <- d2[(np-npMax+1):np,]
                
                #  if(sum( d2$selected > 5) <10)
                #   return(NULL)

                # missing data with average of neighbors
                d2$selected<- meanImput(d2$selected, 2)
                
                #d2$selected <- log10(d2$selected)
                
                # convert to time series using frequency of 7, weekly
                confirm <- ts(d2$selected, frequency=7  )
                
                # forcast
                # https://otexts.com/fpp2/forecasting-decomposition.html
                
                
                forecasted <- stlf(confirm, 
                                   robust = TRUE, 
                                   h = input$daysForcasted, 
                                   level = c(90),
                                   #method = "naive"
                                   #lambda="auto",
                                   method='ets',
                                   etsmodel = "AAN"
                                   #etsmodel = "MAM"    
                                   #biasadj=TRUE,
                                   #damped=TRUE
                                   )
                
                #forecasted$mean <- round(10^forecasted$mean, 0)
                #forecasted$lower <- round(10^forecasted$lower, 0)
                #forecasted$upper <- round(10^forecasted$upper, 0)
                #confirm <- round(10^confirm, 0)
                
                return(list(forecasted = forecasted, confirm = confirm, d2 = d2))
                
                
            }
    } )  
    
 
    
    output$stateProjection <- renderPlot ({
        
        if(is.null(input$selectSeries ))
            return(NULL)
        selected <- input$selectSeries
                
                par(mar = c(6, 4, 2, 2))
                d2 <- projectionData()$d2
                confirm <- projectionData()$confirm
                forecasted <- projectionData()$forecasted
             
                
                # forcast
                # https://otexts.com/fpp2/forecasting-decomposition.html
                
                #ggplot2 version, cannot make date as x-axis label
                if(0){ 
                    autoplot(forecasted) +
                        ylab( selected[1] ) +
                        xlab( paste0(input$selectState, 
                                     " is expected to have ",
                                     round(forecasted$mean[input$daysForcasted],0), 
                                     " ", selected[1], " on ", 
                                     format( as.Date(max(d2$time)) + input$daysForcasted, "%b %d"  ),  
                                     ". 95% CI [",
                                     round(forecasted$lower[input$daysForcasted],0), "-",
                                     round(forecasted$upper[input$daysForcasted],0),"]."
                        )    ) 
                }
                
                
                plot(forecasted, xaxt="n", 
                     ylab = paste0(selected[1], " in ", input$selectState),
                     main = "Trend plus weekly pattern (stlf)")
                
                #construct date sequence
                # https://stackoverflow.com/questions/50924044/decimal-date-to-date-time-using-lubridate-for-daily-time-series-created-by-ts
                a = seq(as.Date(min(d2$time)), 
                        by="days", 
                        length=input$daysForcasted + nrow(d2)  )
                
                # add axis labels in Date format
                axis(1, #below axis
                     las = 3,
                     at = c( time(confirm), time(forecasted$mean)),  # orginal + forecast, 1, 1.1428, 1.28..
                     labels = format(a, "%b %d"))   # Dec. 1
                
            
    })      
    
    output$predictedTable <- renderTable({
        if(is.null(input$selectSeries ))
            return(NULL)
        
        if(is.null(projectionData() ))
            return(NULL)
        
        if(is.null(stateProjectionNoSeasonalData() ))
          return(NULL)
      
        d2 <- projectionData()$d2
        confirm <- projectionData()$confirm
        forecasted <- projectionData()$forecasted
        
        forecasted2 <- stateProjectionNoSeasonalData()$forecasted
        
        a = seq(as.Date(max(d2$time)) + 1, by="days", length=input$daysForcasted  )
        tb <- cbind(as.numeric(forecasted2$mean), as.numeric(forecasted$mean))
        
        tb[tb < 0] <- 0 # replace negative values as zero
        # format 1000 separators
        #https://stackoverflow.com/questions/29465941/format-number-in-r-with-both-comma-thousands-separator-and-specified-decimals
        tb <- format(round(tb, 0), nsmall=0, big.mark=",")  # 1,000.6
        tb <- cbind(format(a, "%b. %d"), 
                    as.character( wday(a, label=TRUE)),
                    tb)
        
        colnames(tb) <- c("Date", "Day", 
                          paste(input$selectSeries[1], "Trend"),
                          paste( "Trend + Weekly Pattern")
                          )
        
        tb
        
    }, include.rownames=FALSE, striped=TRUE, bordered = TRUE, width = "auto", hover=TRUE, align = "r")
    
    
    output$currentStats <- renderText({
      if(is.null(mergedData()))
        return(NULL)
      
      # filter covid data by state
      if(input$selectState == 'US') {
        CTPRegionRaw <- as.data.frame( CTPraw ) %>%
          arrange( desc(date) )
      } else {
        CTPRegionRaw <- as.data.frame( CTPstateRaw ) %>%
          filter(state == input$selectState) %>%
          select(-state) %>%
          arrange( desc(date) )
      }
      
      
      paste( "In", input$selectState,
             "on", format(CTPRegionRaw[1, 'date'], nsmall=0, big.mark=","), ",  ",
             "Confirmed Cases:", format(CTPRegionRaw[1, 'cases'], nsmall=0, big.mark=","),  ",  " ,
             "Hospitalized:", format(CTPRegionRaw[1, 'hospitalized'], nsmall=0, big.mark=","), ",  ",
             "Deaths:", format(CTPRegionRaw[1, 'death'], nsmall=0, big.mark=","))
      
      
      
    })
    
}


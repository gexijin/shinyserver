# for plotting
library(ggplot2)
require(ggrepel)
library(tidyr) # for gather function
library(forcats) # ploting
library(forecast) # time series
library(lubridate) # for showing up time correctly
library(plotly)
library(chinamap)   
library(maps)
    
function(input, output, session) {
    
    observe({  
        cityNamesProvince <- unique( x[input$selectProvince,]$city )
        ix <- match(cityNames, cityNamesProvince)

        updateSelectInput(session, "selectCity", NULL, choices = cityNamesList[!is.na(ix)] ) 
        if( input$selectProvince == entireCountry ) 
            updateSelectInput(session, "selectCity", NULL, choices = NULL )    
        updateSelectInput(session, "selectProvince2", NULL, choices = countriesData()$UScurrent$province )        
        updateSelectInput(session, "selectState", NULL, choices = USCountyData()$UScurrent$province )  
        
        })
  
    observe({
      counties <- USCountyData()$UScumulative %>%
        filter( province == input$selectState) %>%
        arrange(county, desc(confirm)) %>%
        group_by(county) %>%
        filter(row_number() ==1) %>%
        arrange(desc(confirm)) %>% 
        pull(county)
      updateSelectInput(session, "selectCountyUS", NULL, choices = counties) 
    })
    
    output$todayTotalTable <- renderTable(todayTotal,rownames = TRUE, colnames = TRUE, bordered = TRUE)

    #各个省 确诊 历史数  -------------------------------------------    
    output$confirmedByProvincesHistorical <- renderPlot({

            d2 <- xgithub$province %>%
              filter( province != "湖北") %>%
              filter( province != "Hubei") %>%
              filter( country == z("中国")) # after provincal data is added for other countries
            
            if(isEnglish) d2$province <- py2( d2$province )  # translate into Pinyin
            p <- ggplot(d2,
                        aes(time, as.numeric(cum_confirm), group=province, color=province)) +
                geom_point() + geom_line() +
                geom_text_repel(aes(label=province),  family="SimSun",data=d2[d2$time == time(x), ], hjust=1) +
                theme_gray(base_size = 14) + theme(legend.position='none') +
                xlab(NULL) + ylab(NULL) + 
                ggtitle(paste( z(entireCountry),  z("湖北以外"),  xgithub$time ) ) +
                geom_vline(xintercept= as.Date("2020-1-23"), 
                           linetype = "dashed", color = "red", size = 1) +
                annotate(geom="text", x=as.Date("2020-1-24"), y=max(d2$cum_confirm)*.8, label=z("封城"),
                       color="black", hjust = 0) +
                annotate(geom="text", x=as.Date("2020-1-24"), y=max(d2$cum_confirm)*.5, label=z("1月23日"),
                       color="black", hjust = 0)

        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }, width = plotWidth - 100 )
    
    #省内各城市 确诊 历史数  -------------------------------------------    
    output$cities_in_proviences <- renderPlot({

        d <- x[input$selectProvince0, ]
        if(isEnglish) d$city <- py2( d$city )  # translate into Pinyin
        p <- ggplot(d,
               aes(time, as.numeric(cum_confirm), group=city, color=city)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=city), family="SimSun",data=d[d$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) + 
            ggtitle(paste(z(input$selectProvince0), z("各市"),  x$time) ) +
            geom_vline(xintercept= as.Date("2020-1-23"), 
                       linetype = "dashed", color = "red", size = 1) +
            annotate(geom="text", x=as.Date("2020-1-24"), y=max(d$cum_confirm)*.8, label=z("封城"),
                     color="black", hjust = 0) +
            annotate(geom="text", x=as.Date("2020-1-24"), y=max(d$cum_confirm)*.5, label=z("1月23日"),
                     color="black", hjust = 0)
        

        if(input$logScale) 
            p <- p + scale_y_log10() 
        p

    }, width = plotWidth - 100 )

    #全国 当天 确诊 数  -------------------------------------------
    output$realTimeProvinceConfirmed <- renderPlot({

        d = y[]; d <- d[1:20, ]
        d$confirm=as.numeric(d$confirm)
        if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
        d$name = fct_reorder(d$name, d$confirm)        
        
        # This is used to create spaces so the numbers on top of the bar shows up.
        maxN <- max(d$confirm) *1.5
        if(input$logScale) 
            maxN <- max(d$confirm) *10
        
        
        p <- ggplot(d, aes(name, confirm)) + 
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
            theme_gray(base_size=14) + 
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL) +
            theme(text = element_text(size=17, family="SimSun"),
                  axis.text.x = element_text(angle=0, hjust=1))  + 
            #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
            ggtitle(paste( z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
            expand_limits(y = maxN)+ 
          theme(plot.title = element_text(size = 15))
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }, width = plotWidth - 100) 
    
 
  
    
    
       
    #省内各个城市当天确诊数  -------------------------------------------
    output$realTimeCityConfirmed <- renderPlot({
        d = y[input$selectProvince0,] 
        d$confirm=as.numeric(d$confirm)
        if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
        d$name = fct_reorder(d$name, d$confirm)
        
        # This is used to create spaces so the numbers on top of the bar shows up.
        maxN <- max(d$confirm) *1.5
        if(input$logScale) 
            maxN <- max(d$confirm) *10
        
        p <- ggplot(d, aes(name, confirm)) + 
            geom_col(fill='steelblue') + coord_flip() +
            geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
            theme_gray(base_size=14) + 
            scale_y_continuous(expand=c(0,10)) +
            xlab(NULL) + ylab(NULL) +
            theme(text = element_text(size=17, family="SimSun"),
                  axis.text.x = element_text(angle=0, hjust=1))  + 
            #ggtitle(paste("confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
            ggtitle(paste( z(input$selectProvince0), z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
            expand_limits(y = maxN)+ 
          theme(plot.title = element_text(size = 15))
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        p
        
    }, width = plotWidth - 100 ) 
    
    #各个城市死亡率  -------------------------------------------
    output$deathRatesCities <- renderPlotly({
      d = x$data %>% 
        arrange( desc(time, city))  %>%
        filter(!duplicated(city)) %>% 
        filter(province != city ) %>%
        filter(cum_dead > 1)  %>%
        filter(cum_confirm > 50) %>%
        mutate(rate = 100*cum_dead/cum_confirm) 
        

      deathRate = paste0(z("武汉死亡率"), round(d$cum_dead[1]/d$cum_confirm[1]*100 ,2), "%",
                         z(", 其他城市: "), round( mean(d$rate) ,2), "%, [", 
                         round(t.test(d$rate[-1])$conf.int[1],2), "%-",
                         round(t.test(d$rate[-1])$conf.int[2],2),"%]"
                         )      
      
      if(isEnglish) d$province <- py2( d$province )  # translate into Pinyin      
      #if(isEnglish) 
        d$city <- py3( d$city )       
      p <- ggplot(d[-1, ], aes(cum_confirm, cum_dead, color = province, text=city)) +
        xlab(z("各主要城市确诊数")) 

      
      if(input$logScale) {
        p <- ggplot(d[, ], aes(cum_confirm, cum_dead, color = province, text=city)) +
        scale_y_log10() +
        scale_x_log10() +
        xlab(z("各主要城市确诊数")) 

      }
      
      p <- p +
        geom_point(size = 3) + 
        geom_smooth(method = "lm", 
                    inherit.aes = FALSE, 
                    aes(cum_confirm, cum_dead), 
                    se = FALSE, color = "darkgrey",
                    linetype = "dashed") +
        ylab(z("死亡人数")) +
        ggtitle(deathRate) +
        theme(plot.title = element_text(size = 10))
      
     # ggplotly(p, tooltip = c("y", "x","text") ) %>% 
    #    layout( width = plotWidth)
      ggplotly(p, tooltip = c("y", "x","text"),width = plotWidth )
      
    } ) 
    
    
    #各个城市死亡率  -------------------------------------------
    output$deathRatesCitiesCountries <- renderPlotly({
      d = x$data %>% 
        arrange( desc(time, city))  %>%
        filter(!duplicated(city)) %>% 
        filter(province != city ) %>%
        filter (city != "监狱系统") %>%
       # filter(cum_dead > 2)  %>%
        filter(cum_confirm > 200) %>%
        mutate(rate = 100*cum_dead/cum_confirm) %>%
        mutate(isHubei = (province == "Hubei" | province == "湖北")) %>%
        arrange(desc(isHubei), desc(province), desc(cum_confirm) )

      #d <- rbind(d, d[1,]) # move Hunan to the end
      #d <- d[-1, ]
        
      
      if(isEnglish) d$province <- py2( d$province )  # translate into Pinyin      
      if(isEnglish) 
      d$city <- py3( d$city )
      
      d <- d %>% 
        mutate(name = paste0(d$city, ", ", d$province ) )  %>%
        mutate(name = factor(name, levels= rev(name)) )
      
      p <- ggplot(d, aes(x=name, y=rate, color = province)) +
      geom_segment( aes(xend=name, yend=0)) +
      geom_point( size=4, aes( color=province) ) +
      coord_flip() +
      theme_bw() +
      xlab("") +
      ylab(z("死亡率(%)")) + 
      theme(legend.position = "none")
      

      ggplotly(p, tooltip = c("y", "x"), width = plotWidth - 100) 
      
    } ) 
    
    #世界各国死亡率，现在的数据 -------------------------------------------
    output$WorldDeathRate <- renderPlotly({
#      d <- y['global',] %>%
      d <- worldCurrent %>%
        filter(!is.na(name)) %>%
        mutate( confirm =as.numeric(confirm) ) %>%
        #mutate (name = z2( name ) ) %>%
        mutate( name = fct_reorder(name, confirm)) %>% 
        filter( dead > 2 & confirm > 100) %>%
        mutate( deadRate = round(dead / confirm *100,2) ) %>%
        mutate (s = paste0(dead, " / ", confirm)) %>%
        arrange( deadRate) %>%
        mutate(name = factor(name, levels= rev(name)) )
      
      p <- ggplot(d, aes(x=name, y=deadRate, text = s)) +
        geom_segment( aes(xend=name, yend=0)) +
        geom_point( size=3, color = "orange" ) +
        coord_flip() +
        theme_bw() +
        ylab(z("死亡率(%)")) + 
        xlab("")
        theme(legend.position = "none")
      
      
      ggplotly(p, tooltip = c("y", "x", "text"), width = plotWidth - 100, height = 1200)
      

      
      
    } ) 
    
    
    #世界各国分布图，现在的数据 -------------------------------------------
    output$ConfirmedWorld <- renderPlot({
      
     # d <- y['global',] %>%  # Tencent data
      #  filter(!is.na(name)) %>%
      #  mutate( confirm =as.numeric(confirm) ) 
      withProgress(message = 'Making plot', value = 0, {
      d <- worldCurrent # Github data
      
      d <- d %>%
        mutate (name = z2( name ) ) %>%
        mutate( name = fct_reorder(name, confirm))
      
      #d <- d[-1, ] #remove the first row
      d <- d[1:30, ]
      
      
      # This is used to create spaces so the numbers on top of the bar shows up.
      maxN <- max(d$confirm) *1.5
      if(input$logScale) 
        maxN <- max(d$confirm) *20
      
      p <- ggplot(d, aes(name, confirm)) + 
        geom_col(fill='steelblue') + coord_flip() +
        geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
        theme_gray(base_size=14) + 
        scale_y_continuous(expand=c(0,10)) +
        xlab(NULL) + ylab(NULL) +
        theme(text = element_text(size=17, family="SimSun"),
              axis.text.x = element_text(angle=0, hjust=1))  + 
        #ggtitle(paste("Confirmed (deaths) current data from Tencent", gsub(" .*","", y$lastUpdateTime)) ) +
        ggtitle(paste(z("世界各国确诊 (死亡)"), gsub(" .*","", d$time[1]), z(" GitHub")) ) +            
        expand_limits(y = maxN) + 
        theme(plot.title = element_text(size = 15))
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      }) # progress bar
      p
      
    }, width = plotWidth - 100 ) 
    
    
    
    #世界细节 历史图 -------------------------------------------
    output$historicalWorld <- renderPlotly({
      
      tem <- table(xgithub$global$country)
      
      tem2 <- xgithub$global %>%
        group_by(country) %>%
        summarise(max = max(cum_confirm)) %>%
        filter(max > 200) %>%
        pull(country)
      
      d <- xgithub$global %>%
        #filter(country !=z('中国')) %>%
        filter(  country %in%  names(tem)[tem > 20]    ) %>% # only keep contries with 20 more data points.
        filter(  country %in%  tem2   ) %>%  # at least 20 cases
        filter (time > as.Date("2020-2-1"))
      
      p <- ggplot(d,
             aes(time, cum_confirm, group=country, color=country)) +
        geom_point() + geom_line() +
        geom_text_repel(aes(label=country), data=d[d$time == time(x), ], hjust=1) +
        theme_gray(base_size = 12) + #theme(legend.position='none') +
        xlab(NULL) + ylab(NULL) + #xlim(as.Date(c("2020-01-15", "2020-03-01"))) +
        ggtitle ("Confirmed cases") +
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.text=element_text(size=9))   
        #+guides(shape = guide_legend(override.aes = list(size = 2)))
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("y", "x","country"), width = plotWidth) 
      
    })
    
    
    #Deaths across the world -------------------------------------------
    output$historicalWorldDead <- renderPlotly({
      
      tem <- table(xgithub$global$country)
      
      tem2 <- xgithub$global %>%
        group_by(country) %>%
        summarise(max = max(cum_dead)) %>%
        filter(max > 20) %>%
        pull(country)
      
        
      d <- xgithub$global %>%
        #filter(country !=z('中国')) %>%
        filter(  country %in%  names(tem)[tem > 10]    ) %>% # only keep contries with 20 more data points.
        filter(  country %in%  tem2   ) %>%  # at least 20 cases
        filter (time > as.Date("2020-2-1"))
      
      p <- ggplot(d,
                  aes(time, cum_dead, group=country, color=country)) +
        geom_point() + geom_line() +
        geom_text_repel(aes(label=country), data=d[d$time == time(x), ], hjust=1) +
        theme_gray(base_size = 12) + #theme(legend.position='none') +
        xlab(NULL) + ylab(NULL) + #xlim(as.Date(c("2020-01-15", "2020-03-01"))) +
        ggtitle (z("各国死亡人数")) +
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.text=element_text(size=9)) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("y", "x","country"), width = plotWidth) 
      
    })

    #全国细节 历史图 -------------------------------------------
    output$historicalChinaData <- renderPlotly({
      
        dl <- ChinaHistory %>%
            gather( type, count, c(confirm, heal, dead)) %>%
            mutate( type = recode_factor(type,
                                         confirm = z("确诊"),
                                         dead = z("死亡"),
                                         heal = z("痊愈")))

        p <- ggplot(dl,
                    aes(time, count, group=type, color=type)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + #theme(legend.position='none') +
            xlab(NULL) + ylab(NULL)  +
            theme(legend.title = element_blank()) +
            theme(plot.title = element_text(size = 11)) #+
            #geom_vline(xintercept= as.Date("2020-1-23"), 
            #       linetype = "dashed", color = "red", size = 1) +
            #annotate(geom="text", x=as.Date("2020-1-24"), y=60000, label=z("1月23号封城"),
            #       color="black", hjust = 0.5)

            p <- p + ggtitle(paste( z("全国总数"),  x$time) ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth) 
        
    })
    
    #全国细节 历史图 增加-------------------------------------------
    output$historicalChinaDataAdd <- renderPlotly({
        pc <- ChinaHistory
        pc[2:nrow(pc), 2:4] <- (pc[2:nrow(pc), 2:4] / pc[1:(nrow(pc)-1), 2:4] -1 )*100
        pc <- pc[-1, ] %>%
          filter( heal <100) %>%
          filter(time > "2020-1-28")
        
        
        dl <- pc %>%
            gather( type, percentage, c(confirm, heal, dead)) %>%
            mutate( type = recode_factor(type,
                                         confirm = z("确诊"),
                                         dead = z("死亡"),
                                         heal = z("痊愈"))) %>%
            filter( type !=  z("痊愈") )
        p <- ggplot(dl, aes(time, percentage, group=type, color=type)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + #theme(legend.position='none') +
            ylab(NULL) + xlab(NULL) +
            theme(legend.title = element_blank()) +
            theme(plot.title = element_text(size = 11))
        
        p <- p + ggtitle(paste(z("全国每日新增百分比"),  x$time) ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth) 
        
    })

    #全国细节 历史图 增加-------------------------------------------
    output$historicalChinaDataAddRaw <- renderPlotly({
      
      d2 <- ChinaHistory 
      
      d3 <- d2[-1, ] %>%
        mutate(confirm = diff(d2$confirm)) %>%     
        mutate(dead = diff(d2$dead)) %>%
        mutate(heal = diff(d2$heal))
      
      # add a row with zeros but with date; so that the two figures align
      d3 <- rbind(d2[1, ], d3)
      d3[1, 2:4] <- 0;
      
      dl <- d3 %>%
        gather( type, count, c(confirm, heal, dead)) %>%
        mutate( type = recode_factor(type,
                                     confirm = z("确诊"),
                                     dead = z("死亡"),
                                     heal = z("痊愈")))
      p <- ggplot(dl,
                  aes(time, count, group=type, color=type)) +
        geom_point() + geom_line() +
        geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
        theme_gray(base_size = 14) + #theme(legend.position='none') +
        xlab(NULL) + ylab(NULL) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 13))
      
      p <- p + ggtitle(paste(z("全国每日新增"),  x$time) ) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      ggplotly(p, tooltip = c("y", "x"), width = plotWidth)
      
    })    
    
    #省 历史图 新增-------------------------------------------
    output$provienceHistoricalAdd <- renderPlotly({
        d2 <- x[input$selectProvince0, ]  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( city, time, cum_confirm, cum_dead, cum_heal) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
            arrange( order(time)) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) 
        

        d3 <- d2[-1, ] %>%
            mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
            mutate(cum_dead = diff(d2$cum_dead)) %>%
            mutate(cum_heal = diff(d2$cum_heal)) 
        
        # add a row with zeros but with date; so that the two figures align
        d3 <- rbind(d2[1, ], d3)
        d3[1, 2:4] <- 0;
        
        dl <- d3 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = z("确诊"),
                                         cum_dead = z("死亡"),
                                         cum_heal = z("痊愈")))
        
        p <- ggplot(dl,
                    aes(time, count, group=type, color=type)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + #theme(legend.position='none') +
            xlab(NULL) + ylab(NULL)  +
            theme(legend.title = element_blank()) +
          theme(plot.title = element_text(size = 13))
        
        p <- p + ggtitle(paste(z(input$selectProvince0), z("新增"),  x$time) ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth)
        
    })
    
    #省 历史图  -------------------------------------------
    output$provienceHistorical <- renderPlotly({
        d2 <- x[input$selectProvince0, ]  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( city, time, cum_confirm, cum_dead, cum_heal) %>%
            group_by(time) %>%
            summarise( cum_confirm = sum(cum_confirm, na.rm = TRUE), # missing values in some cities
                       cum_dead = sum(cum_dead, na.rm = TRUE),
                       cum_heal = sum(cum_heal,  na.rm = TRUE)) %>%
            arrange( order(time)) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) 
        
        dl <- d2 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = z("确诊"),
                                         cum_dead = z("死亡"),
                                         cum_heal = z("痊愈")))
        
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + 
            #theme(legend.position='none') +
            xlab(NULL) + ylab(NULL)  +
            theme(legend.title = element_blank()) +
          theme(plot.title = element_text(size = 13))
        
        p <- p + ggtitle(paste(z(input$selectProvince0), z("总数"),  x$time) ) 
        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth)
        
    })
            
    #城市细节 历史图 -------------------------------------------
    output$cities_in_proviences_selected <- renderPlotly({

            d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
                mutate(cum_dead = as.integer(cum_dead)) %>%
                select( time, cum_confirm, cum_dead, cum_heal) %>%
                mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
                mutate( cum_dead = meanImput(cum_dead, 2)) %>%
                mutate( cum_heal = meanImput(cum_heal, 2)) 
            
            dl <- d2 %>%
                gather( type, count, cum_confirm:cum_heal) %>%
                mutate( type = recode_factor(type,
                                             cum_confirm = z("确诊"),
                                             cum_dead = z("死亡"),
                                             cum_heal = z("痊愈")))
            
            p <- ggplot(dl,
                        aes(time, count, group=type, color=type)) +
                        geom_point() + geom_line() +
                        #geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
                        theme_gray(base_size = 14) + #theme(legend.position='none') +
                        xlab(NULL) + ylab(NULL) +
                        theme(legend.title = element_blank()) +
                        theme(plot.title = element_text(size = 13))
            p <- p + ggtitle(paste( py1(input$selectCity), z("总数"), z( "更新"), x$time) )                    

        
        if(input$logScale) 
            p <- p + scale_y_log10() 
            
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth)
        
    })
    

    #城市细节 历史图 新增-------------------------------------------
    output$cities_in_proviences_selectedAdd <- renderPlotly({
        
        d2 <- subset(x[input$selectProvince,], city == input$selectCity)  %>% 
            mutate(cum_dead = as.integer(cum_dead)) %>%
            select( time, cum_confirm, cum_dead, cum_heal) %>%
            mutate( cum_confirm = meanImput(cum_confirm, 2)) %>%
            mutate( cum_dead = meanImput(cum_dead, 2)) %>%
            mutate( cum_heal = meanImput(cum_heal, 2)) %>%
            arrange(order(time) )
        
        d3 <- d2[-1, ] %>%
            mutate(cum_confirm = diff(d2$cum_confirm)) %>%     
            mutate(cum_dead = diff(d2$cum_dead)) %>%
            mutate(cum_heal = diff(d2$cum_heal))     
        
        # add a row with zeros but with date; so that the two figures align
        d3 <- rbind(d2[1, ], d3)
        d3[1, 2:4] <- 0;
        
        dl <- d3 %>%
            gather( type, count, cum_confirm:cum_heal) %>%
            mutate( type = recode_factor(type,
                                         cum_confirm = z("确诊"),
                                         cum_dead = z("死亡"),
                                         cum_heal = z("痊愈")))
      
        p <- ggplot(dl,
                    aes(time, as.numeric(count), group=type, color=type)) +
            geom_point() + geom_line() +
            geom_text_repel(aes(label=type), family="SimSun",data=dl[dl$time == time(x), ], hjust=1) +
            theme_gray(base_size = 14) + #theme(legend.position='none') +
            xlab(NULL) + ylab(NULL) +
            theme(legend.title = element_blank())+
            theme(plot.title = element_text(size = 13)) 

            p <- p + ggtitle(paste( py1(input$selectCity), z("新增"),  x$time ) )                   

        
        if(input$logScale) 
            p <- p + scale_y_log10() 
        ggplotly(p, tooltip = c("y", "x"), width = plotWidth) 
    })


    
    
      
    #世界 各国确诊人数预测, 预测-------------------------------------------    
    
      output$forecastConfirmedChangeWorld <- renderPlot ({
        d2 <- contriesPrediction %>%
          arrange(time) %>%
          dplyr::filter( country == input$selectCountry)
        nRep = sum( d2$confirm == d2$confirm[2]) 
        if(nRep > 3) 
          d2 <- d2[-(1:(nRep-3)),]
        
        np <- nrow(d2) # number of time points
        if(np > npMax)
          d2 <- d2[(np-npMax+1):np,]
        
        par(mar = c(4, 4, 0, 2))
        # missing data with average of neighbors
        d2$confirm<- meanImput(d2$confirm, 2)
        
        confirm <- ts(d2$confirm, # percent change
                      start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
        forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), input$daysForcasted)
        plot(forecasted, xaxt="n", main="", 
             ylab = z("确诊人数"),
             xlab = paste0(input$selectCountry, 
                           " is expected to have ",
                           round(forecasted$mean[input$daysForcasted],0), 
                           " confirmed cases by ", 
                           format( as.Date(xgithub$time) + input$daysForcasted, "%b %d"  ),  
                           z(". 95% CI ["),
                           round(forecasted$lower[input$daysForcasted],0), "-",
                           round(forecasted$upper[input$daysForcasted],0),"]."
                           )            
        )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
      }, width = plotWidth - 100 ) 
    
    #世界 各国确诊人数预测, 百分比预测-------------------------------------------      
    output$forecastConfirmedChangeWP <- renderPlot ({
      d2 <- contriesPrediction %>%
        arrange(time) %>%
        filter( country == input$selectCountry)
      nRep = sum( d2$confirm == d2$confirm[2]) 
      if(nRep > 3) 
        d2 <- d2[-(1:(nRep-3)),]
      
      np <- nrow(d2) # number of time points
      if(np > npMax)
        d2 <- d2[(np-npMax+1):np,]
      
      par(mar = c(4, 4, 0, 2)) 
      # missing data with average of neighbors
      d2$confirm<- meanImput(d2$confirm, 2)
      
      np <- nrow(d2) # number of time points
      if(np > npMax)
        d2 <- d2[(np-npMax+1):np,]
      
      confirm <- ts(diff(d2$confirm)/(10 + d2$confirm[1:(nrow(d2)-1)])*100, # percent change
                    start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )
      
      forecasted <- forecast(ets(confirm), input$daysForcasted)
      
      predictedNconfirm = d2$confirm[nrow(d2)]* increasesByPercentages(forecasted$mean)       
      plot(forecasted, xaxt="n", main="", 
           ylab = z("确诊增加百分比(%)"),
           xlab = paste0("Total cases in ", input$selectCountry, 
                         " is expected to increase by ", 
                         round( mean( forecasted$mean ), 1 ),"% on average daily, reaching ", 
                         round(predictedNconfirm,0), " by ",
                         format( as.Date(xgithub$time) + input$daysForcasted, "%b %d"  ), "." 
                         )            
      )
      a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
      axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 )
    
    #世界 各国确诊人数预测, 预测-------------------------------------------    
    
    output$forecastConfirmedChangeWorldDeath <- renderPlot ({
      d2 <- contriesPrediction %>%
        arrange(time) %>%
        filter( country == input$selectCountry)
      nRep = sum( d2$dead == d2$dead[2]) 
      if(nRep > 3) 
        d2 <- d2[-(1:(nRep-3)),]
      
      np <- nrow(d2) # number of time points
      if(np > npMax)
        d2 <- d2[(np-npMax+1):np,]
      
    #  if(sum( d2$dead > 5) <10)
     #   return(NULL)
      
      par(mar = c(4, 4, 0, 2))
      # missing data with average of neighbors
      d2$dead<- meanImput(d2$dead, 2)
      
      confirm <- ts(d2$dead, # percent change
                    start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
      forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), input$daysForcasted)
      plot(forecasted, xaxt="n", main="", 
           ylab = z("COVID-19 Deaths"),
           xlab = paste0(input$selectCountry, 
                         " is expected to have ",
                         round(forecasted$mean[input$daysForcasted],0), 
                         "  deaths by ", 
                         format( as.Date(xgithub$time) + input$daysForcasted, "%b %d"  ),  
                         z(". 95% CI ["),
                         round(forecasted$lower[input$daysForcasted],0), "-",
                         round(forecasted$upper[input$daysForcasted],0),"]."
           )            
      )
      a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
      axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 )  
    
    # compare countries
    output$CompareCountries <- renderPlot ({
      nPoints = 7
      minCases = 200
      maxShow = 30
      tem <- table(xgithub$global$country)
      
      tem2 <- xgithub$global %>%
        group_by(country) %>%
        summarise(max = max(cum_confirm)) %>%
        filter(max > minCases) %>%
        pull(country)
      
      d <- xgithub$global %>%
        filter(  country %in%  names(tem)[tem > nPoints]    ) %>% # only keep contries with 20 more data points.
        filter(  country %in%  tem2   )   # at least 20 cases
      
      dfGroup <- d %>%
        arrange(country, desc(time)) %>%  # only keep the most recent 10 data points
        mutate(cum_confirm = log2(cum_confirm + 1)) %>%
        group_by(country) %>%
        filter(row_number() %in% 1:nPoints) %>%
        arrange(time) %>%
        mutate( time1 =  time - last(time) + nPoints) %>% 
        do(fitGroup = lm(cum_confirm ~ time1, data = .))
      
      Coefs = tidy(dfGroup, fitGroup)
      Coefs <- Coefs %>% 
        filter(term == "time1") %>%
        arrange(desc(estimate)) 
      
      maxConfirm <- d %>%
        arrange(country, desc(time)) %>%  # only keep the most recent 10 data points
        group_by(country) %>%
        filter(row_number() == 1) %>%
        ungroup
      
      Coefs <- left_join(Coefs, maxConfirm, by = "country") %>%
        filter(country != "Diamond Princess") %>%
        mutate( Death.Rate = round(cum_dead/cum_confirm*100,2)) %>%
        mutate ( growthPercent = round((2^estimate -1) *100,2) ) %>%
        arrange(desc(cum_confirm))
      
      Coefs <- Coefs[1:maxShow,]
 
      ggplot(Coefs, aes(x = cum_confirm, y = growthPercent, color = Death.Rate, label = country)) +
        geom_point(size = 2) + 
        scale_x_continuous(trans='log10') +
        geom_text_repel(size = 5, hjust=1) +
        scale_colour_gradient(low = "black", high = "red", na.value = NA) +
        xlab(paste("Confirmed cases as of ", format(as.Date(xgithub$time), "%b. %d")) ) + 
        ylab(paste("% daily increases in last",nPoints,"days" )) + 
        labs(color = "Death Rate") +
        theme_gray(base_size = 12) + 
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.text=element_text(size=11))   
    }, width = plotWidth  )  
    
    #全国确诊人数预测, 百分比预测-------------------------------------------    
    output$forecastConfirmedChange <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2)) 
        # missing data with average of neighbors
        d2$confirm<- meanImput(d2$confirm, 2)
        
        np <- nrow(d2) # number of time points
        if(np > npMax)
          d2 <- d2[(np-npMax+1):np,]
        
        confirm <- ts(diff(d2$confirm)/(10 + d2$confirm[1:(nrow(d2)-1)])*100, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )
        
        forecasted <- forecast(ets(confirm), input$daysForcasted)
        
        predictedNconfirm = d2$confirm[nrow(d2)]* increasesByPercentages(forecasted$mean)       
        plot(forecasted, xaxt="n", main="", 
             ylab = z("全国确诊增加百分比(%)"),
             xlab = paste0(z("预期全国确诊每天增加"), round( mean( forecasted$mean ), 1 ),
                          "%，", input$daysForcasted, z("天后达到 "), round(predictedNconfirm,0) )            
             )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 )
    
    #全国确诊人数预测, 直接预测-------------------------------------------
    output$forecastConfirmedRaw <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))
        # missing data with average of neighbors
        d2$confirm<- meanImput(d2$confirm, 2)
        
        np <- nrow(d2) # number of time points
        if(np > npMax)
          d2 <- d2[(np-npMax+1):np,]
        
        confirm <- ts(d2$confirm, # percent change
                        start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
        forecasted <- forecast(ets(confirm), input$daysForcasted)
        plot(forecasted, xaxt="n", main="", 
             ylab = z("全国确诊"),
             xlab = paste0(z("预期"), input$daysForcasted, z("天后全国确诊 "), round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
                          round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
        )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 ) 

    #全国死亡人数预测, 用百分比预测-------------------------------------------
    output$forecastDeadChange <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))       
        
        # missing data with average of neighbors
        d2$dead <- meanImput(d2$dead, 2)
        
        np <- nrow(d2) # number of time points
        if(np > npMax)
          d2 <- d2[(np-npMax+1):np,]
        
        # Note that 5 is added to the denominator for stablize the %
        dead <- ts(diff(d2$dead)/(5 + d2$dead[1:(nrow(d2)-1)])*100, # percent change
                     start = c(year(min(d2$time)), yday(min(d2$time)) + 1 ), frequency=365  )


        
        forecasted <- forecast(ets(dead), input$daysForcasted)
        
#        predictedNdeaded = d2$dead[nrow(d2)]* (1+ forecasted$mean[input$daysForcasted]/100)^input$daysForcasted 
        predictedNdead = d2$dead[nrow(d2)]* increasesByPercentages(forecasted$mean)           
        plot(forecasted, xaxt="n", main="", 
             ylab = z("死亡人数增加百分比(%)"),
             xlab = paste0(z("预期全国死亡累计每天增加"), round(mean(forecasted$mean),1),
                          "%，", input$daysForcasted, z("天后达到 "), round(predictedNdead,0) )            
        )
        a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 )
    
    #全国死亡人数预测, 直接预测-------------------------------------------
    output$forecastDeadRaw <- renderPlot ({
        d2 <- ChinaHistory
        par(mar = c(4, 3, 0, 2))        
        # missing data with average of neighbors
        d2$dead <- meanImput(d2$dead, 2)
        
        np <- nrow(d2) # number of time points
        if(np > npMax)
          d2 <- d2[(np-npMax+1):np,]
        
        deaded <- ts(d2$dead, # percent change
                     start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
        forecasted <- forecast(ets(deaded), input$daysForcasted)
        plot(forecasted, xaxt="n", main="", 
             ylab = z("全国死亡人数"),
             xlab = paste0(z("预期"), input$daysForcasted, z("天后全国死亡累计"), round(forecasted$mean[input$daysForcasted],0), z(", 区间["),
                          round(forecasted$lower[input$daysForcasted],0), "-",round(forecasted$upper[input$daysForcasted],0),"]")            
        )
        a = seq(as.Date(min(d2$time)), by="days", length= + nrow(d2) -1 )
        axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 )  
    
    #世界地图--------------------------------------------------
    output$worldMap <- renderPlot ({
        withProgress(message = z('下载地图'), value = 0, {
        incProgress(0.1)
        plot(y, 
             continuous_scale=FALSE,
             palette='Blues')
        })
    }, height = 1400, width = 1400)  

    #中国地图---------------------------------------------------
    output$ChinaMap <- renderPlot ({
        withProgress(message = z('下载地图'), value = 0, {
        incProgress(0.1)
        cn = get_map_china()
        cn$province <- trans_province(cn$province) 
        incProgress(0.5)
        })
        plot(get_nCov2019(lang='en'), region='china', chinamap=cn,
             continuous_scale=FALSE,
             palette='Blues')

    }, height = 800, width = 800)   
    
    #省地图---------------------------------------------------
    output$provinceMap <- renderPlot ({
        # 英语版或直辖市不画地图
       # if(isEnglish | input$selectProvince0 %in% specialProvinces) { 
      if(isEnglish ) { 
          return(NULL)
          } else { 
            map1 = sf::st_read("../../data/map/shijie.shp")  
            plot(y, region = input$selectProvince0, 
                 chinamap = map1,
                palette='Blues')  
            }
    }, height = 600, width = 800)  
    

    output$dataDownload <- downloadHandler(
      filename = function() {paste0("coronavirus_histrical_",x$time,".tsv")},
      content = function(file) {
        # issues with Chinese characters solved
        # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
        con <- file(file, open = "w+", encoding = "native.enc")
        df <- x$data
        df$time <- as.character(format(df$time))
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )
    
    output$dataDownloadWorld <- downloadHandler(
      filename = function() {paste0("coronavirus_histrical_",x$time,".tsv")},
      content = function(file) {
        # issues with Chinese characters solved
        # http://kevinushey.github.io/blog/2018/02/21/string-encoding-and-r/
        con <- file(file, open = "w+", encoding = "native.enc")
        df <- xgithub$global
        df$time <- as.character(format(df$time))
        writeLines( paste( colnames(df), collapse = "\t"), con = con, useBytes = TRUE)
        for(i in 1:nrow( df) )
          #write line by line 
          writeLines( paste( df[i,], collapse = "\t"), con = con, useBytes = TRUE)
        close(con)
      }
    )
    
    #世界细节 历史图 -------------------------------------------
    output$historicalWorldDirect <- renderPlotly({
      withProgress(message = 'Making plot', value = 0, {
      library(shadowtext)
      library(conflicted)
      conflict_prefer("filter", "dplyr")
      conflict_prefer("layout", "plotly")   
      incProgress(0.1, message = "loading data")
      d <- xgithub
      dd <- d['global'] %>% 
        as_tibble %>%
        rename(confirm=cum_confirm) %>%
        filter(confirm > 100) %>%
       # filter(confirm <50000) %>%
        filter(country != "Diamond Princess") %>%
        group_by(country) %>%
        mutate(days_since_100 = as.numeric(time - min(time))) %>%
        ungroup 
      
      tem <- dd %>%
        group_by(country) %>%
        summarise(maxDays = max(days_since_100) ) %>%
        arrange(desc(maxDays)) %>%
        as.data.frame()
      
      dd <- dd %>% 
        filter( days_since_100 <= tem[2,2]) %>%  # China has too many days
        filter( country %in% as.character(tem[1:20, 1]) )  # too many countries
      
      
      breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 500000)
      
      incProgress(0.3)
      
      p <- ggplot(dd, aes(days_since_100, confirm, color = country)) +
        # geom_smooth(method='lm', aes(group=1),
        #            data = . %>% filter(!country %in% c("China", "Japan", "Singapore")), 
        #             color='grey10', linetype='dashed') +
        geom_line(size = 0.8) +
        geom_point(pch = 21, size = 1) +
        scale_y_log10(expand = expansion(add = c(0,0.1)), 
                      breaks = breaks, labels = breaks) +
        scale_x_continuous(expand = expansion(add = c(0,1))) +
        theme_gray(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
         # legend.position = "none",
         legend.spacing.y = unit(0.2, 'cm'),
          #plot.margin = margin(3,15,3,3,"mm")
        ) +
        coord_cartesian(clip = "off") +
        geom_shadowtext(aes(label = paste0(" ",country)), hjust=1, vjust = 0, 
                        data = . %>% group_by(country) %>% top_n(1, days_since_100), 
                        bg.color = "white") +
        labs(x = "Number of days since 100th case", y = "" ) + 
        ggtitle (paste0("Confirmed COVID-19 cases as of ", xgithub$time) ) +
        theme(plot.title = element_text(size = 10)) + 
        theme(legend.title = element_blank()) #+
        #xlim(c(0,25))
      }) # progress bar
      ggplotly(p, tooltip = c("y", "x","country"), width = plotWidth) 
      
    })
    
    #世界细节 历史图 -------------------------------------------
    output$historicalWorldDirect2 <- renderPlot({
      
      library(shadowtext)
      library(conflicted)
      
      conflict_prefer("filter", "dplyr")
      conflict_prefer("layout", "graphics")   
      
      d <- xgithub
      dd <- d['global'] %>% 
        as_tibble %>%
        rename(confirm=cum_confirm) %>%
        filter(confirm > 100) %>%
       #  filter(confirm <70000) %>%
        filter(country != "Diamond Princess") %>%
        group_by(country) %>%
        mutate(days_since_100 = as.numeric(time - min(time))) %>%
        ungroup 
      
      tem <- dd %>%
        group_by(country) %>%
        summarise(maxDays = max(days_since_100) ) %>%
        arrange(desc(maxDays)) %>%
        as.data.frame()

      dd <- dd %>% 
        filter( days_since_100 <= tem[2,2]) %>%  # China has too many days
        filter( country %in% as.character(tem[1:20, 1]) )  # too many countries
      
      
      breaks=c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 500000)
      
      
      p <- ggplot(dd, aes(days_since_100, confirm, color = country)) +
        # geom_smooth(method='lm', aes(group=1),
        #            data = . %>% filter(!country %in% c("China", "Japan", "Singapore")), 
        #             color='grey10', linetype='dashed') +
        geom_line(size = 0.8) +
        geom_point(pch = 21, size = 1) +
        scale_y_log10(expand = expansion(add = c(0,0.1)), 
                      breaks = breaks, labels = breaks) +
        scale_x_continuous(expand = expansion(add = c(0,1))) +
        theme_gray(base_size = 14) +
        theme(
          panel.grid.minor = element_blank(),
           legend.position = "none",
          #legend.spacing.y = unit(0.2, 'cm'),
          #plot.margin = margin(3,15,3,3,"mm")
        ) +
        coord_cartesian(clip = "off") +
        geom_shadowtext(aes(label = paste0(" ",country)), hjust=1, vjust = 0, 
                        data = . %>% group_by(country) %>% top_n(1, days_since_100), 
                        bg.color = "white") +
        labs(x = "Number of days since 100th case", y = "", 
             subtitle = paste0("Confirmed COVID-19 cases as of ", xgithub$time, " (static version)") ) 
      
      p
      
    }, width = plotWidth - 100, height = 600 )
    
    ####################################################################
    #  U.S.A. and several other countries with provincal data 
    # based on the coronavirus package from Rami Krispin
    ####################################################################
    
    countriesData <- reactive({
      withProgress(message = "Downloading data.", value = 0, { 
      library(coronavirus)
      data("coronavirus")
      
      # Italy data from covid19Italy package by Rami
      if(input$selectCountryDetails == "Italy") {
        # If Italy use the Italy package to retrieve data. 
        UScurrent <- italy_region %>%
          select(region_name, date, cumulative_positive_cases, death, recovered) %>%
          rename(province = region_name, 
                 confirm = cumulative_positive_cases,
                 dead = death,
                 heal = recovered,
                 time = date) %>%
          arrange(province, desc(time)) %>%
          group_by(province) %>%
          filter(row_number() ==1) %>%
          arrange(desc(confirm))%>%
          filter(confirm > 1)
        
        UScumulative <- italy_region %>%
          select(region_name, date, cumulative_positive_cases, death, recovered) %>%
          rename(province = region_name, 
                 confirm = cumulative_positive_cases,
                 dead = death,
                 heal = recovered,
                 time = date) %>%
          mutate(country = "Italy") %>%
          arrange( province, time)
        
    
      } else if( input$selectCountryDetails %in% nCov2019_countries) {
        # data from nCov-2019 
        # Different countries data from nCov2019 
        if(input$selectCountryDetails == "United States"){
          xgithub$province$province <- gsub("New York state", "New York", xgithub$province$province)
          xgithub$province$province <- gsub("Washington State", "Washington", xgithub$province$province)          
          xgithub$province$province <- gsub("the state of Wisconsin", "Wisconsin", xgithub$province$province)            
          
        }
        
        UScurrent <- xgithub$province %>%
          filter(country == input$selectCountryDetails)  %>% 
          select(province, time, cum_confirm, cum_dead, cum_heal) %>%
          rename(confirm = cum_confirm,
                 dead = cum_dead,
                 heal = cum_heal) %>%
          arrange(province, desc(time)) %>%
          group_by(province) %>%
          filter(row_number() ==1) %>%
          arrange(desc(confirm))%>%
          filter(confirm > 1)      
        
        UScumulative <- xgithub$province %>%
          filter(country == input$selectCountryDetails)  %>% 
          select(province, time, cum_confirm, cum_dead, cum_heal) %>%
          rename(confirm = cum_confirm,
                 dead = cum_dead,
                 heal = cum_heal) %>%
          arrange(province, time)
        
      } else if(input$selectCountryDetails == "US"  ){  # data from New York Times
        NYTdata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
        NYTdata$date <- as.Date(NYTdata$date)
        NYTdata$state <- as.character(NYTdata$state)
        UScurrent <- NYTdata %>%
          rename(province = state, 
                 confirm = cases,
                 dead = deaths,
                 time = date) %>%
          arrange(province, desc(time)) %>%
          group_by(province) %>%
          filter(row_number() ==1) %>%
          arrange(desc(confirm))%>%
          mutate(heal = 0) %>%
          filter(confirm > 1)
        
        UScumulative <- NYTdata %>%
          rename(province = state, 
                 confirm = cases,
                 dead = deaths,
                 time = date) %>%
          mutate(country = "US") %>%
          arrange( province, time)
        
      } else { 
      USdata1 <- coronavirus %>%
         #filter(Country.Region == "US") %>%
        filter(Country.Region == input$selectCountryDetails) %>%
        spread(type, cases) %>% # convert from long to wide format
        arrange(Province.State, date) %>%
        rename(province = Province.State, 
               country = Country.Region,
               time = date,
               confirm = confirmed,
               dead = death) 
      
      #Note that this data records new cases every day.
      UScurrent<- USdata1 %>% 
        drop_na(province) %>%
        filter(province != "NA" ) %>%
        group_by(province) %>%
        summarise(confirm = sum(confirm), 
                  dead = sum(dead), 
                 # heal = sum(heal),
                  time = max(time)) %>% 
        filter(province != "Diamond Princess") %>%
        arrange(desc(confirm))%>%
        filter(confirm > 1)
      
      
      #convert to cumulative numbers
      UScumulative <- USdata1 %>% 
        group_by(province) %>%
        arrange(time) %>%
        mutate( confirm = cumsum(confirm),
                dead = cumsum(dead),
                #heal = cumsum(heal)
                ) %>%
        ungroup() %>%
        arrange( province, time)
      
      }
      
      if(input$selectCountryDetails == "US") { 
          UScumulative$ab <- state.abb[ UScumulative$province] 
      } else {
        UScumulative$ab <- UScumulative$province      
        
      }
      rm(USdata)
      
      }) #progress
      
      return(list(UScurrent = UScurrent, UScumulative = UScumulative))
      
      
    })

    output$USCurrent <- renderPlot({
      withProgress("Downloading data.", value = 0, { 
      d <- countriesData()$UScurrent
      if(nrow(d) > 20) 
        d <- d[1:20, ] 
      d <- d %>%
        rename(name = province)
      
      d$confirm=as.numeric(d$confirm)
      if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
      d$name = fct_reorder(d$name, d$confirm)        
      
      # This is used to create spaces so the numbers on top of the bar shows up.
      maxN <- max(d$confirm) *1.5
      if(input$logScale) 
        maxN <- max(d$confirm) *10
      incProgress(0.2)
      
      p <- ggplot(d, aes(name, confirm)) + 
        geom_col(fill='steelblue') + coord_flip() +
        geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
        theme_gray(base_size=14) + 
        scale_y_continuous(expand=c(0,10)) +
        xlab(NULL) + ylab(NULL) +
        theme(text = element_text(size=17, family="SimSun"),
              axis.text.x = element_text(angle=0, hjust=1))  + 
        ggtitle(paste("Confirmed (deaths) as of", format( as.Date(max(countriesData()$UScumulative$time)), "%b %d") ) ) +
        #ggtitle(paste( z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
        expand_limits(y = maxN)+ 
        theme(plot.title = element_text(size = 15))
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      }) # progress
      p
      
    }, width = plotWidth - 100)  
    
    # us map
    output$US.state.map <- renderPlot({
      library(maps)
      statesData <- map_data("state") 
      UScurrent <- countriesData()$UScurrent
      
      UScurrent$region = tolower(UScurrent$province)
      
      
      map <- merge(statesData, UScurrent, by = "region", all.x = T)
      map <- map[order(map$order), ]
      
      p <- ggplot(map, aes(x = long, y = lat, group = group)) +  
        geom_polygon(aes(fill = confirm)) +   
        geom_path() + 
        #scale_fill_gradientn(colours = rev(heat.colors(10))) +
        scale_fill_gradient2(low = "white", #mid = scales::muted("purple"), 
                             high = "red", breaks = c(10,100,1000,5000,10000,100000)) +
        coord_map() +
        labs(x = "Longitude", y = "Latitude") +
        guides(fill = guide_legend(title = paste0("Confirmed (", 
                                                 format(as.Date(UScurrent$time[1]), "%b. %d"), ")")) ) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) 
       # ggtitle("", subtitle = format(as.Date(UScurrent$time[1]), "%b. %d") )

      p
      
    }, width = plotWidth - 100 )
    
    # us map growth rate
    output$US.state.map.Rate <- renderPlot({
      library(maps)
      statesData <- map_data("state") 
      
      UScurrent <- provinceGrowthRate()
      UScurrent$region = tolower(UScurrent$province)
      
      
      map <- merge(statesData, UScurrent, by = "region", all.x = T)
      map <- map[order(map$order), ]
      
      p <- ggplot(map, aes(x = long, y = lat, group = group)) +  
        geom_polygon(aes(fill = growthPercent)) +   
        geom_path() + 
        #scale_fill_gradientn(colours = rev(heat.colors(10))) +
        scale_fill_gradient2(low = "white", #mid = scales::muted("purple"), 
                             high = "red", breaks = c(0,5,10,20,30,40,50,60,70,80,100)) +
        coord_map() +
        labs(x = "Longitude", y = "Latitude") +
        guides(fill = guide_legend(title = paste0("Daily % increase (", 
                                                  format(as.Date(UScurrent$time[1]), "%b. %d"), ")")) ) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) 
      # ggtitle("", subtitle = format(as.Date(UScurrent$time[1]), "%b. %d") )
      
      p
      
    }, width = plotWidth - 100 )
    
    #US historical by states -------------------------------------------

    output$historicalUS <- renderPlotly({
      
      tem <- countriesData()$UScumulative %>%
        filter(confirm > 0) %>%
        count(province) %>%
        filter( n >= 3 ) %>% # only keep contries with 20 more data points.
        pull(province)
      
      # if no province left return NULL
      if(length(tem) == 0) 
        return(NULL)
      
      tem2 <- countriesData()$UScumulative %>%
        group_by(province) %>%
        summarise(max = max(confirm)) %>%
        filter(max > 20) %>%  # at least 20 cases
        pull(province)
      
      d <- countriesData()$UScumulative %>%
        filter(province !=z('Diamond Princess')) %>%
        filter(  province %in%  tem    ) %>% 
        filter(  province %in%  tem2   ) %>% 
        filter (time > as.Date("2020-2-15"))
      
      p <- ggplot(d,
                  aes(time, confirm, group=province, color=province)) +
        geom_point() + geom_line() +
        geom_text_repel(aes(label=province), data=d[d$time == time(x), ], hjust=1) +
        theme_gray(base_size = 12) + #theme(legend.position='none') +
        xlab(NULL) + ylab(NULL) + #xlim(as.Date(c("2020-01-15", "2020-03-01"))) +
        ggtitle (z("确诊人数")) +
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.text=element_text(size=9))   
      #+guides(shape = guide_legend(override.aes = list(size = 2)))
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("y", "x","province"), width = plotWidth) 
      
    })
   
    
    output$historicalUSDirect2 <- renderPlot({
      
      library(shadowtext)
      library(conflicted)
      
      conflict_prefer("filter", "dplyr")
      conflict_prefer("layout", "graphics")  
      

     dd <- countriesData()$UScumulative %>% 
        as_tibble %>%
        filter(confirm > 20) %>%
        #filter(confirm <50000) %>%
        filter(province != "Diamond Princess") %>%
        filter(province != "Grand Princess") %>%
        group_by(province) %>%
        mutate(days_since_100 = as.numeric(time - min(time))) %>%
        ungroup 
      
     
     tem <- dd %>%
       group_by(province) %>%
       summarise(maxDays = max(days_since_100) ) %>%
       arrange(desc(maxDays)) %>%
       as.data.frame()
     
     dd <- dd %>% 
       filter( province %in% as.character(tem[1:20, 1]) )  # too many states
     
     
     
      if(nrow(dd) <10 ) return(NULL)
      
      breaks=c(20, 50, 100, 200, 500, 1000, 2000, 10000, 50000,100000)

      p <- ggplot(dd, aes(days_since_100, confirm, color = province)) +
        # geom_smooth(method='lm', aes(group=1),
        #            data = . %>% filter(!country %in% c("China", "Japan", "Singapore")), 
        #             color='grey10', linetype='dashed') +
        geom_line(size = 0.8) +
        geom_point(pch = 21, size = 1) +
        scale_y_log10(expand = expansion(add = c(0,0.1)), 
                      breaks = breaks, labels = breaks) +
        scale_x_continuous(expand = expansion(add = c(0,1))) +
        theme_gray(base_size = 14) +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "none",
          #legend.spacing.y = unit(0.2, 'cm'),
          #plot.margin = margin(3,15,3,3,"mm")
        ) +
        coord_cartesian(clip = "off") +
        geom_shadowtext(aes(label = paste0(" ",ab)), hjust=0, vjust = 0, 
                        data = . %>% group_by(province) %>% top_n(1, days_since_100), 
                        bg.color = "white") +
        labs(x = "Days since the 20th case", y = "", 
             subtitle = paste0("Confirmed COVID-19 cases as of ", 
                               format(as.Date(max(countriesData()$UScumulative$time)),"%b. %d")) )+
        xlim(c(0,(floor(max(dd$days_since_100)/10) + 1)*10) )
      
      p
      
    }, width = plotWidth - 100 )
    
    provinceGrowthRate <- reactive({
      minCases = 50 
      
      UScumulative <- countriesData()$UScumulative
      tem <- table(UScumulative$province)
      
      tem2 <- UScumulative %>%
        group_by(province) %>%
        summarise(max = max(confirm)) %>%
        filter(max > minCases) %>%
        pull(province)
      
      # if no province left return NULL
      if(length(tem2) == 0) 
        return(NULL)
      
      d <- UScumulative %>%
        filter(  province %in%  names(tem)[tem >= nPoints]    ) %>% # only keep contries with 20 more data points.
        filter(  province %in%  tem2   )   # at least 20 cases
      
      if(nrow(d) == 0) 
        return(NULL)
      
      dfGroup <- d %>%
        arrange(province, desc(time)) %>%  # only keep the most recent 10 data points
        mutate(confirm = log2(confirm + 1)) %>%
        group_by(province) %>%
        filter(row_number() %in% 1:nPoints) %>%
        arrange(time) %>%
        mutate( time1 =  time - last(time) + nPoints) %>% 
        do(fitGroup = lm(confirm ~ time1, data = .))
      
      Coefs = tidy(dfGroup, fitGroup)
      Coefs <- Coefs %>% 
        filter(term == "time1") %>%
        arrange(desc(estimate)) 
      
      maxConfirm <- d %>%
        arrange(province, desc(time)) %>%  # only keep the most recent 10 data points
        group_by(province) %>%
        filter(row_number() == 1) %>%
        ungroup
      
      Coefs <- left_join(Coefs, maxConfirm, by = "province") %>%
        filter(province != "Diamond Princess") %>%
        mutate( Death.Rate = round(dead/confirm*100,2)) %>%
        mutate ( growthPercent = round((2^estimate -1) *100,2) ) %>%
        arrange(desc(confirm))
      
      return(Coefs)
      
      
    })
    
    
    
    # compare Provinces
    output$CompareProvinces <- renderPlot ({
      minCases = 50
      maxShow = 50
      
      Coefs <- provinceGrowthRate()
      #write.csv(Coefs, "tem.csv")
      if(is.null(Coefs))
        return(NULL)
      if(nrow(Coefs) > maxShow)
      Coefs <- Coefs[1:maxShow,]
      
      ggplot(Coefs, aes(x = confirm, y = growthPercent, color = Death.Rate, label = ab)) +
        geom_point(size = 2) + 
        scale_x_continuous(trans='log10') +
        geom_text_repel(size = 6, hjust=1) +
        scale_colour_gradient(low = "black", high = "red", na.value = NA) +
        xlab(paste("Confirmed cases as of ", format(as.Date(xgithub$time), "%b. %d")) ) + 
        ylab(paste("% daily increases in last",nPoints,"days" )) + 
        labs(color = "Death Rate") +
        theme_gray(base_size = 12) + 
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.text=element_text(size=11))  +
        theme(axis.text=element_text(size=12),
             axis.title=element_text(size=12))
    }, width = plotWidth  )  
    
    # Prediction U.S. states
    output$forecastUSstates <- renderPlot ({
      d2 <- countriesData()$UScumulative %>%
        arrange(time) %>%
        filter( province == input$selectProvince2)
      
      if(nrow(d2) < 5) return(NULL)

      
      np <- nrow(d2) # number of time points
      if(np > npMax)
        d2 <- d2[(np-npMax+1):np,]
      
      par(mar = c(4, 4, 0, 2))
      # missing data with average of neighbors
      d2$confirm<- meanImput(d2$confirm, 2)
      
      confirm <- ts(d2$confirm, # percent change
                    start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
      forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), input$daysForcasted2)
      plot(forecasted, xaxt="n", main="", 
           ylab = z("确诊人数"),
           xlab = paste0(input$selectProvince2, 
                         " is expected to have ",
                         round(forecasted$mean[input$daysForcasted2],0), 
                         " confirmed cases by ", 
                         format( as.Date(max(d2$time)) + input$daysForcasted2, "%b %d"  ),  
                         z(". 95% CI ["),
                         round(forecasted$lower[input$daysForcasted2],0), "-",
                         round(forecasted$upper[input$daysForcasted2],0),"]."
           )            
      )
      a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted2 + nrow(d2) -1 )
      axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 ) 
    
    
#----------------------------------
    #Italy
    output$ItalyActiveCases <- renderPlotly({
      #library(conflicted)
      #conflict_prefer("filter", "dplyr")
      #conflict_prefer("layout", "plotly")  
      p <- plot_ly(data = italy_total,
              x = ~ date,
              y = ~home_confinement, 
              name = 'Home Confinement', 
              fillcolor = '#FDBBBC',
              type = 'scatter',
              mode = 'none', 
              stackgroup = 'one',
              width = plotWidth) %>%
        add_trace( y = ~ hospitalized_with_symptoms, 
                   name = "Hospitalized with Symptoms",
                   fillcolor = '#E41317') %>%
        add_trace(y = ~intensive_care, 
                  name = 'Intensive Care', 
                  fillcolor = '#9E0003') %>%
        plotly::layout(title = "Italy - Distribution of Active Covid19 Cases" ,
               legend = list(x = 0.1, y = 0.9),
              yaxis = list(title = "Number of Cases"),
               xaxis = list(title = "Source: Italy Department of Civil Protection"))
   
      # plotly::layout to prevent error due to conflicting with the layout function in base graphics
         
      if(input$logScale) 
        p <- plotly::layout(p, yaxis = list(type = "log"))

        p

      
    })
    
    output$ItalyByRegion <- renderPlotly({

      p <- italy_region %>% 
        filter(date == max(date)) %>% 
        select(region_name, cumulative_positive_cases, recovered, death, cumulative_positive_cases) %>%
        arrange(-cumulative_positive_cases) %>%
        mutate(region = factor(region_name, levels = region_name)) %>%
        plot_ly(y = ~ region, 
                x = ~ cumulative_positive_cases, 
                orientation = 'h',
                text =  ~ cumulative_positive_cases,
                textposition = 'auto',
                type = "bar", 
                name = "Active",
                marker = list(color = "#1f77b4"), 
                width = plotWidth
                ) %>%
        add_trace(x = ~ recovered,
                  text =  ~ recovered,
                  textposition = 'auto',
                  name = "Recovered",
                  marker = list(color = "forestgreen")) %>%
        add_trace(x = ~ death, 
                  text =  ~ death,
                  textposition = 'auto',
                  name = "Death",
                  marker = list(color = "red"))  %>%
        plotly::layout(title = "Cases Distribution by Region",
               barmode = 'stack',
               yaxis = list(title = "Region"),
               xaxis = list(title = "Number of Cases"),
             hovermode = "compare",
               legend = list(x = 0.65, y = 0.9),
           margin =  list(
                 l = 20,
                 r = 10,
               b = 10,
                t = 30,
                 pad = 2
               ))
      if(input$logScale) 
        p <- plotly::layout(p, xaxis = list(type = "log"))

        p

    }) 
    
    output$ItalyProvinceDist <- renderPlotly({

      italy_province %>% 
        filter(date == max(date), region_name == input$ItalyRegion) %>%
        plot_ly(labels = ~province_name, values = ~total_cases, 
                textinfo="label+percent",
                type = 'pie',
                width = plotWidth) %>%
        #layout(title = input$ItalyRegion ) %>% 
        hide_legend()
      
      
    }) 
    
    
    #--------US county level-----------------------------------------------
    # Data from New York Times
    #----------------------------------------------------------------------
    
    USCountyData <- reactive({
      withProgress(message = "Downloading data.", value = 0, { 
        library(coronavirus)
        data("coronavirus")
        
          NYTdata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
          NYTdata$date <- as.Date(NYTdata$date)
          NYTdata$state <- as.character(NYTdata$state)
          UScurrent <- NYTdata %>%
            rename(province = state, 
                   confirm = cases,
                   dead = deaths,
                   time = date) %>%
            arrange(province, county, desc(time)) %>%
            group_by(province, county) %>%
            filter(row_number() ==1) %>%
            arrange(desc(confirm))%>%
            mutate(heal = 0) %>%
            filter(confirm > 1)
          
          UScumulative <- NYTdata %>%
            rename(province = state, 
                   confirm = cases,
                   dead = deaths,
                   time = date) %>%
            mutate(country = "US") %>%
            arrange( province, time)

          UScumulative$ab <- state.abb[ UScumulative$province] 
   
        
      }) #progress
      
      return(list(UScurrent = UScurrent, UScumulative = UScumulative))
      
    })
 
    USCountyData <- reactive({
      withProgress(message = "Downloading data.", value = 0, { 
        library(coronavirus)
        data("coronavirus")
        
        NYTdata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
        NYTdata$date <- as.Date(NYTdata$date)
        NYTdata$state <- as.character(NYTdata$state)
        UScurrent <- NYTdata %>%
          rename(province = state, 
                 confirm = cases,
                 dead = deaths,
                 time = date) %>%
          arrange(province, county, desc(time)) %>%
          group_by(province, county) %>%
          filter(row_number() ==1) %>%
          arrange(desc(confirm))%>%
          mutate(heal = 0) %>%
          filter(confirm > 1)
        
        UScumulative <- NYTdata %>%
          rename(province = state, 
                 confirm = cases,
                 dead = deaths,
                 time = date) %>%
          mutate(country = "US") %>%
          arrange( province, time)
        
        UScumulative$ab <- state.abb[ UScumulative$province] 
        
        
      }) #progress
      
      return(list(UScurrent = UScurrent, UScumulative = UScumulative))
      
    })   
    
    output$USCountyDataNYT <- renderPlot({
    # The current US data at the county level from NYT
      d <- USCountyData()$UScurrent %>%
        filter(province == input$selectState)
      if(nrow(d) > 20) 
        d <- d[1:20, ] 
      d <- d %>%
        rename(name = county)
      
      if(is.null(d)) return(NULL)
      if(nrow(d) < 1) return(NULL)      
      
      d$confirm=as.numeric(d$confirm)
      if(isEnglish) d$name <- py2( d$name )  # translate into Pinyin
      d$name = fct_reorder(d$name, d$confirm)        
      
      # This is used to create spaces so the numbers on top of the bar shows up.
      maxN <- max(d$confirm) *1.5
      if(input$logScale) 
        maxN <- max(d$confirm) *10
      
      
      p <- ggplot(d, aes(name, confirm)) + 
        geom_col(fill='steelblue') + coord_flip() +
        geom_text(aes(y = confirm+2, label= paste0( confirm, " (",dead,")")), hjust=0) +
        theme_gray(base_size=14) + 
        scale_y_continuous(expand=c(0,10)) +
        xlab(NULL) + ylab(NULL) +
        theme(text = element_text(size=17, family="SimSun"),
              axis.text.x = element_text(angle=0, hjust=1))  + 
        ggtitle(paste("Cases (deaths) as of", format( as.Date(max(USCountyData()$UScumulative$time)), "%b. %d") ) ) +
        #ggtitle(paste( z("确诊 (死亡)"), gsub(" .*","", y$lastUpdateTime), z("腾迅")) ) +            
        expand_limits(y = maxN)+ 
        theme(plot.title = element_text(size = 15))
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      p
      
    }, width = plotWidth - 100)  
    
    
    output$historicalUSCounty <- renderPlot({
      
      library(shadowtext)
      library(conflicted)
      
      conflict_prefer("filter", "dplyr")
      conflict_prefer("layout", "graphics")  
      
      dd <- USCountyData()$UScumulative %>%
        filter(province == input$selectState) %>%
     # dd <- UScumulative %>%
     #   filter(province == "New York")  %>%
        as_tibble %>%
        filter(confirm > 20) %>%
        group_by(county) %>%
        mutate(days_since_100 = as.numeric(time - min(time))) %>%
        ungroup 
      
      tem <- dd %>%
        group_by(county) %>%
        summarise(maxDays = max(days_since_100) ) %>%
        arrange(desc(maxDays)) %>%
        as.data.frame()
      
      if(nrow(tem) > 20)
         dd <- dd %>% 
            filter( county %in% as.character(tem[1:20, 1]) )  # too many states
      
      if(nrow(dd) <5 ) return(NULL)
      
      breaks=c(20, 50, 100, 200, 500, 1000, 2000, 10000, 50000,100000)
      
      p <- ggplot(dd, aes(days_since_100, confirm, color = county)) +
        # geom_smooth(method='lm', aes(group=1),
        #            data = . %>% filter(!country %in% c("China", "Japan", "Singapore")), 
        #             color='grey10', linetype='dashed') +
        geom_line(size = 0.8) +
        geom_point(pch = 21, size = 1) +
        scale_y_log10(expand = expansion(add = c(0,0.1)), 
                      breaks = breaks, labels = breaks) +
        scale_x_continuous(expand = expansion(add = c(0,1))) +
        theme_gray(base_size = 14) +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "none", 
          #legend.spacing.y = unit(0.2, 'cm'),
          #plot.margin = margin(3,15,3,3,"mm")
        ) +
        coord_cartesian(clip = "off") +
        geom_shadowtext(aes(label = paste0(" ",county)), hjust=0, vjust = 0, 
                        data = . %>% group_by(county) %>% top_n(1, days_since_100), 
                        bg.color = "white") +
        labs(x = "Days since the 20th case", y = "", 
             subtitle = paste0("Confirmed COVID-19 cases as of ", 
                               format(as.Date(max(USCountyData()$UScumulative$time)),"%b. %d")) )+
        xlim(c(0,(floor(max(dd$days_since_100)/10) + 1)*10) )
      
      p
      
    }, width = plotWidth - 100 )
    
    
    USCountyGrowthRate <- reactive({
      # clculate growth rates for US counties
      minCases = 20 
      
      UScumulative <- USCountyData()$UScumulative %>%
        filter(province == input$selectState) %>%
        droplevels()
        
      # UScumulative <- UScumulative %>%       
      #  filter(province == "New York") %>%
      #  droplevels()
        
      tem <- table(UScumulative$county)
      
      tem2 <- UScumulative %>%
        group_by(county) %>%
        summarise(max = max(confirm)) %>%
        filter(max > minCases) %>%
        pull(county)
      
      # if no county left return NULL
      if(length(tem2) == 0) 
        return(NULL)
      
      d <- UScumulative %>%
        filter(  county %in%  names(tem)[tem >= nPoints]    ) %>% # only keep contries with 20 more data points.
        filter(  county %in%  tem2   ) %>%  # at least 20 cases
        droplevels()
      
      if(nrow(d) == 0) 
        return(NULL)
      
      dfGroup <- d %>%
        arrange(county, desc(time)) %>%  # only keep the most recent 10 data points
        mutate(confirm = log2(confirm + 1)) %>%
        group_by(county) %>%
        filter(row_number() %in% 1:nPoints) %>%
        arrange(time) %>%
        mutate( time1 =  time - last(time) + nPoints) %>% 
        do(fitGroup = lm(confirm ~ time1, data = .))
      
      Coefs = tidy(dfGroup, fitGroup)
      Coefs <- Coefs %>% 
        filter(term == "time1") %>%
        arrange(desc(estimate)) 
      
      maxConfirm <- d %>%
        arrange(county, desc(time)) %>%  # only keep the most recent 10 data points
        group_by(county) %>%
        filter(row_number() == 1) %>%
        ungroup
      
      Coefs <- left_join(Coefs, maxConfirm, by = "county") %>%
        mutate( Death.Rate = round(dead/confirm*100,2)) %>%
        mutate ( growthPercent = round((2^estimate -1) *100,2) ) %>%
        arrange(desc(confirm))
      
      return(Coefs)
      
      
    })
    
    # compare Provinces
    output$CompareUScounties <- renderPlot ({
      minCases = 20
      maxShow = 30
      
      Coefs <- USCountyGrowthRate()
      if(is.null(Coefs))
        return(NULL)
      if(nrow(Coefs) > maxShow)
        Coefs <- Coefs[1:maxShow,]
      
      ggplot(Coefs, aes(x = confirm, y = growthPercent, color = Death.Rate, label = county)) +
        geom_point(size = 2) + 
        scale_x_continuous(trans='log10') +
        geom_text_repel(size = 6, hjust=1) +
        scale_colour_gradient(low = "black", high = "red", na.value = NA) +
        xlab(paste("Confirmed cases as of ", format( as.Date(max(USCountyData()$UScumulative$time)), "%b. %d")  ) ) + 
        ylab(paste("% daily increases in last",nPoints,"days" )) + 
        labs(color = "Death Rate") +
        theme_gray(base_size = 12) + 
        theme(plot.title = element_text(size = 12)) + 
        theme(legend.text=element_text(size=11))  +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=12))
    }, width = plotWidth  )  
    
    # Prediction U.S. states
    output$forecastUScounties <- renderPlot ({

      d2 <- USCountyData()$UScumulative %>%
        filter(province == input$selectState, county == input$selectCountyUS) %>%
        arrange(time) %>%
        droplevels()
      
      if(nrow(d2) < 5) return(NULL)
      
      
      np <- nrow(d2) # number of time points
      if(np > npMax)
        d2 <- d2[(np-npMax+1):np,]
      
      par(mar = c(4, 4, 0, 2))
      # missing data with average of neighbors
      d2$confirm<- meanImput(d2$confirm, 2)
      
      confirm <- ts(d2$confirm, # percent change
                    start = c(year(min(d2$time)), yday(min(d2$time))  ), frequency=365  )
      forecasted <- forecast(ets(confirm, model="AAN", damped=FALSE), input$daysForcasted3)
      plot(forecasted, xaxt="n", main="", 
           ylab = z("确诊人数"),
           xlab = paste0(input$selectCountyUS, 
                         " is expected to have ",
                         round(forecasted$mean[input$daysForcasted3],0), 
                         " confirmed cases by ", 
                         format( as.Date(max(d2$time)) + input$daysForcasted3, "%b %d"  ),  
                         z(". 95% CI ["),
                         round(forecasted$lower[input$daysForcasted3],0), "-",
                         round(forecasted$upper[input$daysForcasted3],0),"]."
           )            
      )
      a = seq(as.Date(min(d2$time)), by="days", length=input$daysForcasted3 + nrow(d2) -1 )
      axis(1, at = decimal_date(a), labels = format(a, "%b %d"))
    }, width = plotWidth - 100 ) 
    
    output$US.county.map <- renderPlot({
      # county maps in the U.S.
      library(usmap)

      UScurrent <- USCountyData()$UScurrent %>%
        filter(province == input$selectState)
      
      #UScurrent <- UScurrent %>%
      #  filter(province == input$selectState)   
      
      countyData <- UScurrent[, c("fips","county","confirm")]
      countyData <- countyData[!is.na(countyData$fips),]
      
      p <- usmap::plot_usmap(regions = "counties", include = c(input$selectState), labels = TRUE,
                        data = countyData, values = "confirm") + 
       # labs(subtitle = "These are the states in the Pacific Timezone.") +
        theme(legend.position = "right") + 
        scale_fill_gradient2(low = "white", high = "red",  trans = "log10", label = scales::comma) +
        guides(fill = guide_legend(title = paste0("Confirmed (", 
                                                format(as.Date(UScurrent$time[1]), "%b. %d"), ")")) ) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) 
 
      p
      
    }, width = plotWidth, height = 700)
    
    output$US.county.mapRate <- renderPlot({
      # county maps in the U.S.
      library(usmap)
      if(is.null(USCountyGrowthRate() )) return(NULL)
      UScurrent <- USCountyGrowthRate() %>%
        filter(province == input$selectState)
      
      #UScurrent <- UScurrent %>%
      #  filter(province == input$selectState)   
      
      countyData <- UScurrent[, c("fips","county","growthPercent")]
      countyData <- countyData[!is.na(countyData$fips),]
      
      p <- usmap::plot_usmap(regions = "counties", include = c(input$selectState), labels = TRUE,
                             data = countyData, values = "growthPercent") + 
        # labs(subtitle = "These are the states in the Pacific Timezone.") +
        theme(legend.position = "right") + 
        scale_fill_gradient2(low = "white", high = "red", label = scales::comma) +
        guides(fill = guide_legend(title = paste0("Daily % increase (", 
                                                  format(as.Date(UScurrent$time[1]), "%b. %d"), ")")) ) +  
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank()) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()) 
      
      p
      
    }, width = plotWidth, height = 700)
    

    output$percentIncreaseUScounty <- renderPlotly({
      if(is.null(USCountyData() )) return(NULL)
      pc <- USCountyData()$UScumulative %>%
        filter(province == input$selectState, county == input$selectCountyUS) %>%
        arrange(time) %>%
        droplevels()
      
      if(dim(pc) <5) return(NULL)

      pc0 <- pc
      pc <- pc[-1, ] # delete the first row
      pc0 <- pc0[-nrow(pc0) ,] #delete the last row
      pc$Deaths <-  round((pc$dead / pc0$dead -1 )*100, 1)
      pc$Cases <-  round((pc$confirm / pc0$confirm -1 )*100, 1)    
    
      np <- nrow(pc) # number of time points
      if(np > npMax)
        pc <- pc[(np-npMax+1):np,]
      
      dl <- pc %>%
        gather( type, percentage, c(Cases, Deaths))
      
      p <- ggplot(dl, aes(time, percentage, group=type, color=type)) +
        geom_point() + geom_line() +
        #geom_text_repel(aes(label=type), data=dl[dl$time == time(x), ], hjust=1) +
        theme_gray(base_size = 14) + #theme(legend.position='none') +
        ylab(NULL) + xlab(NULL) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 11))
      
      p <- p + ggtitle(paste("%daily increases in", input$selectCountyUS) ) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("type", "y", "x"), width = plotWidth) 
      
    })
    

    output$percentIncreaseUState <- renderPlotly({
      # daily % increases in states
      
      if(is.null(USStateData())) return(NULL)
      pc <- USStateData()$UScumulative %>%
        filter(province == input$selectState) %>%
        arrange(time) %>%
        droplevels()
      if(dim(pc) <5) return(NULL)
      pc0 <- pc
      pc <- pc[-1, ] # delete the first row
      pc0 <- pc0[-nrow(pc0) ,] #delete the last row
      pc$Deaths <-  round((pc$dead / pc0$dead -1 )*100, 1)
      pc$Cases <-  round((pc$confirm / pc0$confirm -1 )*100, 1)    
      
      np <- nrow(pc) # number of time points
      if(np > npMax)
        pc <- pc[(np-npMax+1):np,]
      pc <- pc %>%
        filter( Deaths < 100, Cases < 100)
      
      dl <- pc %>%
        gather( type, percentage, c(Cases, Deaths))
      
      p <- ggplot(dl, aes(time, percentage, group=type, color=type)) +
        geom_point() + geom_line() +
       # geom_text_repel(aes(label=type), data=dl[dl$time == time(x), ], hjust=1) +
        theme_gray(base_size = 14) + #theme(legend.position='none') +
        ylab(NULL) + xlab(NULL) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 11))
      
      p <- p + ggtitle(paste("Percent daily increases in", input$selectState) ) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("type", "y", "x"), width = plotWidth) 
      
    })
    
    output$IncreasesUState <- renderPlotly({
      # daily increases in states
      if(is.null(USStateData())) return(NULL)
      pc <- USStateData()$UScumulative %>%
        filter(province == input$selectState) %>%
        arrange(time) %>%
        droplevels()
      if(dim(pc) <5) return(NULL)
      pc0 <- pc
      pc <- pc[-1, ] # delete the first row
      pc0 <- pc0[-nrow(pc0) ,] #delete the last row
      pc$Deaths <-  pc$dead - pc0$dead  
      pc$Cases <-  pc$confirm - pc0$confirm     
      
      np <- nrow(pc) # number of time points
      if(np > npMax)
        pc <- pc[(np-npMax+1):np,]

      
      dl <- pc %>%
        gather( type, Numbers, c(Cases, Deaths))
      
      p <- ggplot(dl, aes(time, Numbers, fill=type)) +
        geom_bar(stat = "identity", position = 'dodge', colour="black") + 
        theme_gray(base_size = 14) + #theme(legend.position='none') +
        ylab(NULL) + xlab(NULL) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 11))
      
      p <- p + ggtitle(paste("Daily new cases and deaths in", input$selectState) ) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("type", "y", "x"), width = plotWidth) 
      
    })  
    
    output$IncreasesUSCounty <- renderPlotly({
      # daily increases in county
      if(is.null(USCountyData())) return(NULL)
      
      pc <- USCountyData()$UScumulative %>%
        filter(province == input$selectState, county == input$selectCountyUS) %>%
        arrange(time) %>%
        droplevels()
      if(dim(pc) <5) return(NULL)
      pc0 <- pc
      pc <- pc[-1, ] # delete the first row
      pc0 <- pc0[-nrow(pc0) ,] #delete the last row
      pc$Deaths <-  pc$dead - pc0$dead  
      pc$Cases <-  pc$confirm - pc0$confirm     
      
      np <- nrow(pc) # number of time points
      if(np > npMax)
        pc <- pc[(np-npMax+1):np,]
      
      
      dl <- pc %>%
        gather( type, Numbers, c(Cases, Deaths))
      
      p <- ggplot(dl, aes(time, Numbers, fill=type)) +
        geom_bar(stat = "identity", position = 'dodge', colour="black") + 
        theme_gray(base_size = 14) + #theme(legend.position='none') +
        ylab(NULL) + xlab(NULL) +
        theme(legend.title = element_blank()) +
        theme(plot.title = element_text(size = 11))
      
      p <- p + ggtitle(paste("Daily new cases and deaths in", input$selectCountyUS) ) 
      
      if(input$logScale) 
        p <- p + scale_y_log10() 
      
      ggplotly(p, tooltip = c("type", "y", "x"), width = plotWidth) 
      
    })   
    
    USStateData <- reactive({
      # state level data for the U.S.
      NYTdata <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
      NYTdata$date <- as.Date(NYTdata$date)
      NYTdata$state <- as.character(NYTdata$state)
      UScurrent <- NYTdata %>%
        rename(province = state, 
               confirm = cases,
               dead = deaths,
               time = date) %>%
        arrange(province, desc(time)) %>%
        group_by(province) %>%
        filter(row_number() ==1) %>%
        arrange(desc(confirm))%>%
        mutate(heal = 0) %>%
        filter(confirm > 1)
      
      UScumulative <- NYTdata %>%
        rename(province = state, 
               confirm = cases,
               dead = deaths,
               time = date) %>%
        mutate(country = "US") %>%
        arrange( province, time)
      
      return(list(UScurrent = UScurrent, UScumulative = UScumulative))
      
    })   
    
    
    
    #----------------------------------------------------------------------------
    # mobility data
    GoogleMobilityData <- reactive({
      URL = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
      withProgress({ 
        incProgress(0.2, "Downloading data. This can take 5 minutes")
        res <- read.csv(URL)
      })
      res
    })
    
    CountryMoility <- renderPlotly({
      
      p <- GoogleMobilityData() %>%
        filter(iso2c == "US" & admin_level==0) %>%
        ggplot(aes(x=date,y=percent_change_from_baseline,color = places_category)) +
        geom_line() +
        #ggtitle('Google mobility metric for US') +
        theme(legend.position='bottom')
      ggplotly(p)
      
      
    })
    
    
    
    

}

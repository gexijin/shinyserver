# Shiny app for showing the number of confirmed coronavirus cases across China
# Xijin Ge 2/5/2020 

library(shiny)
library(plotly)
library(shinyBS,verbose=FALSE) # for popup figures
# Define server logic required to draw a histogram
ui <- fluidPage(
  titlePanel(paste0(z("疫情统计和预测")," v0.86")),
  tabsetPanel(
    tabPanel(z("世界")
             ,fluidRow( 
               column(6, checkboxInput("logScale", z("所有的图对数坐标 log10"), value = FALSE) )
               ,column(6, align="right",a(z("英语"),  href=myURL) )
             )
             ,img(src='worldMap.jpg', align = "center",width="700", height="400")
              ,br(),br()
              ,h4("Most plots are updated daily. Downloading the latest data ......")
              ,plotOutput("ConfirmedWorld", height = 500)
              ,h5(legends[8])
              ,br(),br()
              ,plotlyOutput("historicalWorld")
              ,h5(legends[9])
              ,br(),br()
              ,plotlyOutput("historicalWorldDead")
              ,h5(legends[10])
              ,br(),br()
              ,plotlyOutput("historicalWorldDirect")
              ,h5(legends[11])
              ,br(),br()
              ,plotOutput("historicalWorldDirect2", height = 600)
              ,h5(legends[12])
              ,br()
              ,plotOutput("CompareCountries")
              ,h5(legends[22])
              ,br(),br()
              ,plotlyOutput("WorldDeathRate", height = 1200)
              ,h5(legends[13])
              ,br(),br()
              
              
              
    )#tab2 --------------------------------------------------
    ,tabPanel(z("各国")

              ,selectInput("selectCountryDetails", NULL, choices = countriesDetail, selected = "US")
              ,conditionalPanel("input.selectCountryDetails == 'US'"
                                ,h5("Data from the", a("New York Times.", href="https://github.com/nytimes/covid-19-data") ) )
              ,conditionalPanel("input.selectCountryDetails != 'US'"              
                        ,h5("Please wait while we retrieve today's data. All analyses on this page are based on data from this ", 
                  a("R package",href="https://github.com/RamiKrispin/coronavirus"), 
                  "by", a("Rami Krispin.",href="https://twitter.com/rami_krispin?lang=en")))
             ,plotOutput("USCurrent")
              ,h5(legends[17])
              ,br(),br() 
              ,conditionalPanel("input.selectCountryDetails == 'US'"
                                ,plotOutput("US.state.map")
                                #,img(src='US_March22_total.jpg', align = "center",width="770", height="440")
                                ,h5(legends[18])
                                ,br(),br()    
                                #,img(src='US_March22_rate.jpg', align = "center",width="770", height="440")
                                ,plotOutput("US.state.map.Rate")
                                ,h5(legends[24])
                                ,br(),br()    
              )
              
              
              ,plotlyOutput("historicalUS")
              ,h5(legends[19])
              ,br(),br() 
              
              ,plotOutput("historicalUSDirect2")
              ,h5(legends[20])
              ,br(),br() 
              ,plotOutput("CompareProvinces")
              ,h5(legends[23])
              ,br(),br()          
              ,sliderInput("daysForcasted2", paste(z("选择预测天数") ),
                           min = 1, max = 14,
                           value = 7)
              ,selectInput("selectProvince2", NULL, NULL)
              ,plotOutput("forecastUSstates")
              ,h5(legends[21])
              ,br(),br() 
              
              
    )

    
    ,tabPanel(z("预测") 
              ,sliderInput("daysForcasted", paste(z("选择预测天数"), gsub("2020-","", xgithub$time))  ,
                           min = 1, max = 14,
                           value = 7)
              ,selectInput("selectCountry", NULL, choices = countryNames, selected= countryNames[2])
              
              ,plotOutput("forecastConfirmedChangeWorld")
              ,h5(legends[14])
              ,br(),br()
              ,plotOutput("forecastConfirmedChangeWP")
              ,h5(legends[15])
              ,br(),br()     
              ,plotOutput("forecastConfirmedChangeWorldDeath")
              ,h5(legends[16])
              ,br(),br() 
              
              
    ) #tab2 --------------------------------------------------
    ,tabPanel(z("美国")
              ,h5("Loading county-level historical data from the", a("New York Times.", href="https://github.com/nytimes/covid-19-data") )
              ,selectInput("selectState", NULL, NULL)

              ,plotlyOutput("IncreasesUState")   
              ,plotlyOutput("percentIncreaseUState")
              
          
              ,plotOutput("historicalUSCounty")
              ,h5(legends[26])
              ,br(),br() 
              ,plotOutput("USCountyDataNYT")
              ,h5(legends[25])
              ,br(),br() 
              ,plotOutput("US.county.map", inline = TRUE)
              ,h5(legends[27])
              ,br(),br() 
              ,plotOutput("US.county.mapRate", inline = TRUE)
              ,h5(legends[28])
              ,br(),br() 
              #,plotOutput("historicalUSDirect2")
              ,plotOutput("CompareUScounties")
              ,h5(legends[29])
              ,br(),br()          
              ,sliderInput("daysForcasted3", paste(z("选择预测天数") ),
                           min = 1, max = 14,
                           value = 7)
              ,selectInput("selectCountyUS", NULL, NULL)
                            ,plotOutput("forecastUScounties")
              ,h5(legends[30])
              ,br(),br() 
              #,h5(legends[21])
              ,plotlyOutput("IncreasesUSCounty")             
              ,plotlyOutput("percentIncreaseUScounty")
              #,br(),br() 
              
              
    )
    ,tabPanel(z("意大利")
              ,h5("All analyses on this page are based on code and data from this ", 
                  a("R package",href="https://github.com/RamiKrispin/covid19Italy"), 
                  "by", a("Rami Krispin.",href="https://twitter.com/rami_krispin?lang=en"))
              ,plotlyOutput("ItalyActiveCases") 
              ,br(),br() 
              ,plotlyOutput("ItalyByRegion") 
              ,br(),br() 
              ,selectInput("ItalyRegion", NULL, choices = ItalyRegions )
              ,plotlyOutput("ItalyProvinceDist")
              ,br(),br() 
    )
    ,tabPanel(z("全国")
    ,h4( paste0( 
                 z("全国确诊:"), y$chinaTotal$confirm, " (", finc(y$chinaAdd$confirm), ", ", finc( round(y$chinaAdd$confirm/(y$chinaTotal$confirm - y$chinaAdd$confirm)*100,1)  ), "%)",
                 z(",   疑似:"), y$chinaTotal$suspect, " (", finc(y$chinaAdd$suspect), ", ", finc( round(y$chinaAdd$suspect/(y$chinaTotal$suspect - y$chinaAdd$suspect)*100,1)  ), "%)",
                 z(",   死亡:"), y$chinaTotal$dead,    " (", finc(y$chinaAdd$dead),    ", ", finc( round(y$chinaAdd$dead/(y$chinaTotal$dead - y$chinaAdd$dead)*100,1)  ), "%)",
                z(",   痊愈:"),  y$chinaTotal$heal,    " (", finc(y$chinaAdd$heal),    ", ", finc( round(y$chinaAdd$heal/(y$chinaTotal$heal - y$chinaAdd$heal)*100,1)  ), "%)"
                 )
         )
    
    ,h5(    z("更新"), "     ", z( paste0( gsub("-.*","", gsub(" .*|2020-","",y$lastUpdateTime)), "月")),
            gsub(".*-","", gsub(" .*|2020-","",y$lastUpdateTime)), z("日"),"  ", z("北京时间"), "  ", gsub(".* ","", y$lastUpdateTime),". ",
            z("一天之内数字会有多次更新。") )
    
    #,plotlyOutput("historicalChinaDataPlotly")
    ,img(src='ChinaMapAnimated2.gif', align = "center",width="600", height="600")
     ,h5(legends[1])
     ,br(),br()
    ,plotOutput("realTimeProvinceConfirmed")
     ,h5(legends[2])
     ,br()
    ,plotlyOutput("historicalChinaData")
    ,h5(legends[3])
     ,br()
    ,plotlyOutput("historicalChinaDataAdd")
     ,h5(legends[4])
    ,br()
    ,plotOutput("confirmedByProvincesHistorical")    
    ,h5(legends[5])
    , br()
    ,plotlyOutput("deathRatesCities")
    ,h5(legends[6])
    ,br()
    ,plotlyOutput("deathRatesCitiesCountries")
    ,h5(legends[7])

  


    ,br()

    ) #tab1 --------------------------------------------------
    

    ,tabPanel(z("省")
              ,selectInput("selectProvince0", NULL, choices = provinceNamesList)
    #,tableOutput("todayTotalTable")
    ,plotOutput("realTimeCityConfirmed") 
    ,br()
    ,plotlyOutput("provienceHistorical")      
    ,plotlyOutput("provienceHistoricalAdd")      
    ,plotOutput("cities_in_proviences")    
    ,br()
    #,plotlyOutput("cities_in_proviences_selected_plotly")

    ,plotOutput("provinceMap") 
    ,br()
    )
    
    ,tabPanel(z("市")
              ,fluidRow( 
                column(3, selectInput("selectProvince", NULL, choices = provinceNamesList)  ),
                column(3, selectInput("selectCity", NULL, choices = cityNames))
              )
              ,plotlyOutput("cities_in_proviences_selected")        
              ,plotlyOutput("cities_in_proviences_selectedAdd")  
              )
,tabPanel(z("地图")
          ,h4(paste0( z(paste0(gsub("-.*","", gsub(" .*|2020-","",y$lastUpdateTime)), "月"))," ",
                      gsub(".*-","", gsub(" .*|2020-","",y$lastUpdateTime)), z("日")), z("(稍等几秒钟，地图下载)。"))
          ,plotOutput("ChinaMap", height = 800, width = 800)
          ,h4("Scroll down to see world map.")
          ,plotOutput("worldMap", height = 1400, width = 1400)    
)
,tabPanel("Mobility" 
          ,br()
          , plotlyOutput("CountryMoility")
           
          ,br()
          
) #tab2 --------------------------------------------------
    ,tabPanel(z("数据") 
              ,br()
              , downloadButton('dataDownload', z('中国数据下载')	)
              ,br(),br()
              , downloadButton('dataDownloadWorld', z('世界数据下载')	)             
              ,br()
              
    ) #tab2 --------------------------------------------------
    ,tabPanel("About"
    ,h5("For feedbacks or suggestions  please contact me via "
        ,a("email ",href="mailto:xijin.ge@sdstate.edu?Subject=Coronavirus website" ), "or", 
        a("Twitter.", href="https://twitter.com/StevenXGe"),
        "My research interests are genomics, bioinformatics, and data science ",
        a("(lab homepage).", href="http://ge-lab.org/"), 
        "Source code on ", a("GitHub.",aref="https://github.com/gexijin/wuhan"), 
        " I am not a epidemiologists or statistician, so be critical of my analyses.",

        "I am just a college professor having fun in a basement during spring break. But  I am enjoying it more 
        than my students on the Florida beach ...")
        
     ,h5("Accuracy not guaranteed. Part of the data is not official statistics.", style = "color:red")   
    ,h5("For details on data sources see our", a("preprint.", href="https://www.medrxiv.org/content/10.1101/2020.02.25.20027433v1"))
    #,h5("All rights reserved.") 
    ,h5("This website visualizes the data and trend of the 2019-nCoV (SARS-Cov-2) coronavirus pandemic. Developed",
        "based on these R packages:", a("nCov2019",href="https://github.com/GuangchuangYu/nCov2019"), 
        "by", a("Dr. Guangchuang Yu,", href="https://twitter.com/guangchuangyu"), 
        a("coronavirus",href="https://github.com/RamiKrispin/coronavirus"), "and",
        a("covid19Italy",href="https://github.com/RamiKrispin/covid19Italy"),
        "by", a("Rami Krispin.",href="https://twitter.com/rami_krispin?lang=en")
        ,"U.S. state and county level data from the", a("New York Times.", href="https://github.com/nytimes/covid-19-data")  
        )    

    
    ,h4("不保证数据和分析的可靠性，仅供参考。", style = "color:red")
    ,h5("该网站是我工作之余仓促码出来的, 难免有错误。见",
        a("源代码。 ", href="https://github.com/gexijin/wuhan"),
        "主要目的是帮助朋友们了解疫情。纯粹个人行为。",
        "bcloud.org 是以前注册的一个域名，随手拿来用了，不属于任何组织。"
    )
    ,h5("之所以能很快写出来，最主要是因为南方医科大学的",
        a("余光创教授",  href="http://portal.smu.edu.cn/jcyxy/info/1084/2203.htm"),
        "(微信公众号biobabble）写了一个功能强大的下载实时数据的软件包：",
        a("nCov2019。", href="https://mp.weixin.qq.com/s?__biz=MzI5NjUyNzkxMg==&mid=2247488621&idx=1&sn=727f8bdec2801ddc0315b9fedaa40acc&scene=21"),
        "实时数据来自腾讯， 每天更新。历史数据来源：丁香园，国家卫健委，和",
        a("GitHub.",href="https://github.com/canghailan/Wuhan-2019-nCoV"), "数据每天更新。")
    ,h5("有意见或建议可以给我发"
        ,a("邮件",href="mailto:xijin.ge@sdstate.edu?Subject=疫情网站" ),"。 ",
        "我做生物信息学方面的研究，用计算的方法探索生命的奥秘 (",
        a("研究室网页", href="http://ge-lab.org/"), ")。"  )
    ,h5("武汉加油！ 中国加油！")
    
    ,h5("2/5/20  Version 0")  
    ,h5("2/8/20  Version 0.1")
    ,h5("2/9/20 Version 0.2 ")
    ,h5("2/12/20 V 0.3 English version")
    ,h5("2/23/20 v. 0.4 Interactive plots.")
    ,h5("3/12/20 v. 0.5 Historical trend among countries.")
    ,h5("3/15/20 V. 0.6 Changed forecasting parameters from default to ANN.")
    ,h5("3/16/20 V. 0.7 Add provincial level data for U.S. and other countries based on the coronavirus package.")
    ,h5("3/20/20 V. 0.8 Add Italy data")
    ,h5("3/27/20 V. 0.8 Add us data from the New York Times.")
    ,h5("3/28/20 V. 0.8 Add detailed county level data from the New York Times.")
        )
  )
    ,tags$head(includeScript("ga.js")) # tracking usage with Google analytics      
)
library(shiny)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(DT)
library(shinythemes)
library(shinydashboard)
library(dygraphs)
library(ggplot2)
library(fpp3)

RETAIL <- aus_retail %>%
  mutate(Month = yearmonth(Month)) %>% 
  update_tsibble(index = Month) %>% 
  as_tsibble(index = Month, regular = TRUE) 

server <- shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({
    {
      aus_retail %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        mutate(Month = yearmonth(Month)) %>% 
        update_tsibble(index = Month) %>% 
        as_tsibble(index = Month, regular = TRUE) %>% 
        mutate(diff = difference(Turnover)) %>%
        autoplot()
    }
  }) 
  
  output$interp <- renderPrint({
    if(input$diff == "Seasonality"){
      "This graph is looking at seasonality. When looking at these graphs if the means are relatively the same, meaning that the blue line on the graph is at the same height on all of the graphs, then there is weak seasonality. If there is a large difference between the blue lines of the graphs then that shows strong seasonality."} 
    else if(input$diff == "Autocorrelation"){
      "This graph is looking at Autocorrelation using the ACF. This shows the linear relationship between lagged values of a time series. When looking at this graph the dashed blue lines show if the correlations are significantly greater than zero. If the black lines do not go past the blue lines then there is a weak seasonal pattern. If the lines exceed the blue dashed lines there is usually a pattern and it means that seasonality exists. An example of this is a peak every 12 months shown well through the department store input."} 
    else{
      "This graph is looking at White Noise. When looking at these plots, you want the residual plot to have homoscedasticity, meaning equal spread throughout. When looking at the ACF plot you want the lines to stay inside of the blue dashed lines. Finally when looking at the histogram, you want it to look normal, meaning that it has a bell shaped curve."
    }
  })
  
  output$interp2 <- renderPrint({
    if(input$dec == "Additive"){
      "This graph is looking at the Classical Additive Decomposition. The first graph shows the regular plot of the data. The secons plot shows the trend of the data, so when the graph goes up there is an increasing trend, while if it goes down there is a decreasing trend. The next plot shows seasonality, so if there is a pattern of increases and decreases that means that you have seasonality. Finally the last plot shows the remainder, or what is left over. When looking at this plot you want the dime, the grey bar at the begenning of the graph to be very large, and you want the other dimes to be very small. When this happens it means that most of the variation is explained through the trend and seasonality which is a great thing."} 
    else{
      "This graph is looking at the Classical Multiplicative Decomposition. The first graph shows the regular plot of the data. The secons plot shows the trend of the data, so when the graph goes up there is an increasing trend, while if it goes down there is a decreasing trend. The next plot shows seasonality, so if there is a pattern of increases and decreases that means that you have seasonality. Finally the last plot shows the remainder, or what is left over. When looking at this plot you want the dime, the grey bar at the begenning of the graph to be very large, and you want the other dimes to be very small. When this happens it means that most of the variation is explained through the trend and seasonality which is a great thing."
    }
  })
  
  output$plot2 <- renderPlot({
    if(input$diff == "Seasonality"){
      aus_retail %>%
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        mutate(Month = yearmonth(Month)) %>% 
        update_tsibble(index = Month) %>% 
        as_tsibble(index = Month, regular = TRUE) %>% 
        mutate(diff = difference(Turnover)) %>% 
        gg_subseries() +
        labs(title = "Sesonality using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$diff == "Autocorrelation"){
      aus_retail %>%
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        mutate(Month = yearmonth(Month)) %>% 
        update_tsibble(index = Month) %>% 
        as_tsibble(index = Month, regular = TRUE) %>% 
        mutate(diff = difference(Turnover)) %>% 
        ACF(diff) %>% 
        autoplot() +
        labs(title = "Autocorrelation using a Correlogram of Australia's Retail Turnover")
    } else{
      aus_retail %>%
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        mutate(Month = yearmonth(Month)) %>% 
        update_tsibble(index = Month) %>% 
        as_tsibble(index = Month, regular = TRUE) %>% 
        mutate(diff = difference(Turnover)) %>% 
        model(NAIVE = MEAN(diff)) %>% 
        gg_tsresiduals() +
        labs(title = "White Noise Plots of Australia's Retail Turnover")
    } 
    
  })
  
  output$plot3 <- renderPlot({
    if(input$dec == "Additive"){
      aus_retail %>%
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(classical_decomposition(Turnover, type = "additive")) %>%
        components() %>%
        autoplot() +
        labs(title = "Classical Additive Decomposition of Australia's Retail Turnover")
    } else {
      aus_retail %>%
        filter(State == input$state, Industry == input$industry) %>%
        model(classical_decomposition(Turnover, type = "multiplicative")) %>%
        components() %>%
        autoplot() +
        labs(title = "Classical Multiplicative Decomposition of Australia's Retail Turnover")
    }
  })
  
  output$plot4 <- renderPlot({
    if(input$basic == "NAIVE"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(Naive = NAIVE(Turnover)) %>% 
        forecast(h = input$forecast, level=95) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "NAIVE forecast using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$basic == "Seasonal NAIVE"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(SNAIVE = SNAIVE(Turnover)) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Seasonal NAIVE forecast using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$basic == "Drift"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(Drift = RW(Turnover ~ drift())) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Drift forecast using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$basic == "Linear"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(Linear = TSLM(Turnover ~ trend())) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Linear forecast using a Subseries Plot of Australia's Retail Turnover")
    } else{
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(Mean = MEAN(Turnover)) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Mean forecast using a Subseries Plot of Australia's Retail Turnover")
    } 
    
  })
  
  
  output$plot5 <- renderPlot({
    if(input$ETS == "Holts"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(additive = ETS(Turnover ~ error("A") + trend("A") +
                               season("A"))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Exponential Smoothing forecast using Holts' as a Subseries Plot of Australia's Retail Turnover")
    } else {
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(multiplicative = ETS(Turnover ~ error("M") + trend("A") +
                                     season("M"))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Exponential Smoothing forecast using Holt's Winters as a Subseries Plot of Australia's Retail Turnover")
    }
  })
  
  output$plot6 <- renderPlot({
    if(input$arima == "ARIMA210"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(arima210 = ARIMA(Turnover ~ pdq(2,1,0))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "ARIMA forecast with the values of pdq(2,1,0) using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$arima == "ARIMA013"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(arima013 = ARIMA(Turnover ~ pdq(0,1,3))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "ARIMA forecast with the values of pdq(0,1,3) using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$arima == "ARIMA000011"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(arima000011 = ARIMA(Turnover ~ pdq(0,0,0) + PDQ(0,1,1))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "ARIMA forecast with seasonal componet with the values of pdq(0,0,0), PDQ(0,1,1) using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$arima == "ARIMA210011"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(arima210011 = ARIMA(Turnover ~ pdq(2,1,0) + PDQ(0,1,1))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "ARIMA forecast with seasonal componet with the values of pdq(2,1,0), PDQ(0,1,1) using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$arima == "Stepwise"){
      RETAIL %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(stepwise = ARIMA(Turnover)) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "ARIMA Stepwise forecast using a Subseries Plot of Australia's Retail Turnover")
    } else if (input$arima == "Auto"){
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(auto = ARIMA(Turnover, stepwise = FALSE, approx = FALSE)) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Auto ARIMA forecast using a Subseries Plot of Australia's Retail Turnover")
    } else{
      RETAIL %>% 
        filter(Month >= input$date[1] & Month <= input$date[2]) %>% 
        filter(State == input$state, Industry == input$industry) %>%
        model(arima = ARIMA(Turnover ~ pdq(input$p,input$d,input$q))) %>% 
        forecast(h = input$forecast) %>%
        autoplot(RETAIL %>% filter(Month >= input$date[1] & Month <= input$date[2])) +
        labs(title = "Your own ARIMA forecast using a Subseries Plot of Australia's Retail Turnover")
      
    } 
    
  })
})








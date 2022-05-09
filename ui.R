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
library(dashboardthemes)
library(shinythemes)
library(rsconnect)

data("aus_retail")
retail <- aus_retail
RETAIL <- aus_retail %>%
  mutate(Month = yearmonth(Month)) %>% 
  update_tsibble(index = Month) %>% 
  as_tsibble(index = Month, regular = TRUE)

train <- RETAIL %>% 
  filter(Month <= yearmonth("2005 Jan"))

holdout <- RETAIL %>% 
  filter(Month > yearmonth("2005 Jan"))


ui <- dashboardPage(dashboardHeader(title="Exploring Australian Retail"), 
                    dashboardSidebar(id="",sidebarMenu(
                      menuItem(menuSubItem("Instructions",tabName = "Inst")),
                      menuItem(menuSubItem(text="Australian Stock Market News", icon = icon("send",lib='glyphicon'), href = "https://www.businessnewsaustralia.com/")),
                      menuItem(menuSubItem("Choose Your Item",tabName = "Ind")),
                      menuItem(text="Let's plot!",icon=icon("chart-line"),
                               menuSubItem("Plotting!",tabName = "ST1"),
                               menuSubItem("Decomposition",tabName = "ST2")),
                      menuItem(text = "Let's Forecast!", icon = icon("chart-line"),
                               menuSubItem("Forecast Length?", tabName = "ST3"),
                               menuSubItem("Basic Models!",tabName = "ST4"),
                               menuSubItem("ETS",tabName = "ST5"),
                               menuSubItem("ARIMA",tabName = "ST6")))),
                    
                    dashboardBody(tabItems(
                      tabItem(
                        tabName = "Inst",
                        fluidPage(
                          theme = shinytheme("journal"),
                          tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #f77f00;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #ffffff;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #f77f00;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #f77f00;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #f77f00;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #f77f00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ffffff;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ffffff;
                              }
                              '))),
                          h1("Welcome to my app! Through my app you will be exploring the Austrailian Retail Industry", align = "center"),
                          h3("Once you click on the \"Australian Stock Market News\" tab, you will be redirected to a web page that shows the latest Australian Stock Market News", align = "center"),
                          h3("Under the \"Choose Your Item\" tab you will be able to pick the Australian Region, along with being able to chose the retail industry that you want to explore. You will also be able to see the date range that you want to see. Once you chose your preferences the graph will change to show your choice over time.", align = "center"),
                          h3("Under the \"Let's Plot!\" tab you will be able to pick plotting many types of graphs including Seasonality, AutoCorelation, White Noise, and both Additive and Multiplicative Decomposition", align = "center"),
                          h3("Under the \"Plotting\" sub tab you will be able to pick between Seasonality, AutoCorelation, and White Noise. You have to click on one of the choices to see the graph. Whenever you want to see the next plot you have to click on that one that you previously clicked then click on the graph that you want to see next", align = "center"),
                          h3("Under the \"Decomposition\" sub tab you will be able to pick between Additive and Multiplicative Decomposition. You have to click on one of the choices to see the graph. Whenever you want to see the next plot you have to click on that one that you previously clicked then click on the graph that you want to see next", align = "center"),
                          h3("Under the \"Let's Forecast!\" tab you will be able to forecast through many techniques", align = "center"),
                          h3("Under the \"Forecast Length?\" tab you can use the slider input to choose how many months in the future that you would like to forecast", align = "center"),
                          h3("Under the \"Basic Models!\" sub tab you will  be able to look at the NAIVE, Seasonal NAIVE, Drift, Linear, and the MEAN models of forecasting.", align = "center"),
                          h3("Under the \"ETS\" sub tab you will be using Exponential Smoothing technique of both Holt's and Holt's Winters to get your forecast", align = "center"), 
                          h3("Under the \"ARIMA\" sub tab you will be using ARIMA models to get your forecast. There will be pre-made ARIMA models like the ARIMA210, but there will also be an AUTO ARIMA model and a Stepwise ARIMA Model. Finally you will also be able to make your own ARIMA model selecting the pdq through the number input and selecting \"ARIMA\"", align = "center")
                        )),
                      
                      tabItem(
                        tabName = "Ind",
                        selectInput(
                          "industry",
                          h3("Select an industry"),
                          choices = names(table(retail$Industry)),
                          selected = 1
                        ),
                        fluidRow(column(3, verbatimTextOutput("value"))),
                        selectInput(
                          "state",
                          h3("Select an Australian State"),
                          choices = names(table(retail$State)),
                          selected = 1
                        ),
                        fluidRow(column(3, verbatimTextOutput("value2"))),
                        dateRangeInput("date", "Filter by Month and Year:",
                                       start  = "1982-05-01",
                                       end    = "2018-12-31",
                                       min    = "1982-05-01",
                                       max    = "2018-12-31",
                                       format = "mm/yyyy"),
                        plotOutput(outputId = "plot1")
                      ),
                      tabItem(tabName = "ST1",
                              checkboxGroupButtons(
                                inputId = "diff",
                                label = "Choices", 
                                choices = c("Seasonality", "Autocorrelation", "White Noise"),
                                selected = "Seasonality",
                                status = "danger"
                              ),
                              plotOutput(outputId = "plot2"),
                              verbatimTextOutput("interp"
                              )),
                      tabItem(tabName = "ST2",
                              checkboxGroupButtons(
                                inputId = "dec",
                                label = "Choices", 
                                choices = c("Additive", "Multiplicative"),
                                selected = "Additive",
                                status = "danger"
                              ),
                              plotOutput(outputId = "plot3"),
                              verbatimTextOutput("interp2"
                              )),
                      tabItem(tabName = "ST3",
                              numericInput(inputId = "forecast", label = "How Many Months Would you Like to Forecast in the Future?", value = 24, min = 1, max = NA, step = 1,
                                           width = NULL
                              ),),
                      tabItem(tabName = "ST4",
                              checkboxGroupButtons(
                                inputId = "basic",
                                label = "Choices", 
                                choices = c("NAIVE", "Seasonal NAIVE", "Drift","Linear","MEAN"),
                                selected = "NAIVE",
                                status = "danger"
                              ),
                              plotOutput(outputId = "plot4"),
                      ),
                      tabItem(tabName = "ST5",
                              checkboxGroupButtons(
                                inputId = "ETS",
                                label = "Choices", 
                                choices = c("Holts", "Holts Winters"),
                                selected ="Holts",
                                status = "danger"
                              ),
                              plotOutput(outputId = "plot5"),
                      ),
                      tabItem(tabName = "ST6",
                              checkboxGroupButtons(
                                inputId = "arima",
                                label = "Choices", 
                                choices = c("ARIMA210", "ARIMA013", "ARIMA000011", "ARIMA210011", "Stepwise","Auto","ARIMA"),
                                selected = "ARIMA210",
                                status = "danger"
                              ),
                              numericInput("p","Input p value for \"ARIMA\"",value = 1,min = 0, max = 3),
                              br(),
                              numericInput("d","Input d value for \"ARIMA\"",value = 1,min = 0, max = 1),
                              br(),
                              numericInput("q","Input p value for \"ARIMA\"",value = 1,min = 0, max = 3),
                              plotOutput(outputId = "plot6"),
                      ))))


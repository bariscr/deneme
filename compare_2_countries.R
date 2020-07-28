library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rio)

# load data ----
covid_data <- rio::import('https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.xlsx',
                 col_types = c("text", "text", "text", 
                               "text", "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "text", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric", "numeric", 
                               "numeric", "numeric"))

ui <-   dashboardPage(
  skin = "purple",
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Name this", tabName = "tab_1", icon = icon("file-alt")),
    menuItem("Name this also", tabName = "tab_2", icon = icon("file-alt"))
  )),
  
  # dashboard start ----
  dashboardBody(tabItems(
    
    # password tab ----
    tabItem("tab_1",
            fluidPage(
              h1("COVID COMPARISON"),
              selectInput("country1", "Select country 1", choices = unique(covid_data$location)),
              selectInput("country2", "Select country 2", choices = unique(covid_data$location)),
              selectInput("variable", "Select a variable to compare", 
                          choices = c("Total Cases" = "total_cases", "New Cases" = "new_cases",
                                      "Total Deaths" = "total_deaths", "New Deaths" = "new_deaths",
                                      "Total Cases per Million" = "total_cases_per_million", 
                                      "New cases Per Million" = "new_cases_per_million", 
                                      "Total Deaths per Million" = "total_deaths_per_million", 
                                      "New Deaths per Million" = "new_deaths_per_million")),
              dateRangeInput("daterange", "Select a date range", start = "2019-12-31", end = Sys.Date()),
              tableOutput("tablo"),
              mainPanel({
                plotOutput("graph", brush = "click")
              })
            )),
    
    tabItem("tab_2",
            fluidPage(
              h1("Move it babe")
 )))))


server <- function(input, output) {
  
  data <- reactive({
    covid_data %>% filter(location == input$country1 | location == input$country2, date >= input$daterange[1], date <= input$daterange[2])
  })
  output$tablo <- renderTable({
    brushedPoints(data(), input$click, yvar = input$variable)
  })
  output$graph <- renderPlot({
    ggplot(data(), aes(x = date, y = .data[[input$variable]], color = location)) + geom_point()
  })
}

shinyApp(ui, server)

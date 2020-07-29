library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rio)
library(scales)
library(purrr)

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

covid_data$date <- as.Date(covid_data$date)

ui <-   dashboardPage(
  skin = "purple",
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Country COVID-19 Profile", tabName = "tab_1", icon = icon("heart")),
    menuItem("Name this also", tabName = "tab_2", icon = icon("file-alt")),
    menuItem("Name this also2", tabName = "tab_3", icon = icon("file-alt"))
  )),
  
  # dashboard start ----
  dashboardBody(tabItems(
    
    # tab 1----
    tabItem("tab_1",
            fluidRow(
              selectInput("country_select_tab1", "Select Country", choices = unique(covid_data$location)),
              dateInput("date_tab1", "Select a Date", value = Sys.Date()),
              infoBoxOutput("approvalBox"),
              infoBoxOutput("approvalBox2")
            )),    

    tabItem("tab_2",
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
    
    tabItem("tab_3",
            fluidPage(
              selectInput("country_alone", "Select country", choices = unique(covid_data$location)),
              selectInput("variable_compare", "Select variables to compare", choices = c("Total Cases" = "total_cases", "New Cases" = "new_cases",
                                                                                         "Total Deaths" = "total_deaths", "New Deaths" = "new_deaths",
                                                                                         "Total Cases per Million" = "total_cases_per_million", 
                                                                                         "New cases Per Million" = "new_cases_per_million", 
                                                                                         "Total Deaths per Million" = "total_deaths_per_million", 
                                                                                         "New Deaths per Million" = "new_deaths_per_million"), multiple = TRUE),
              dateRangeInput("daterange_alone", "Select a date range", start = "2019-12-31", end = Sys.Date()),
              plotOutput("plot_alone")
 ))
)))


server <- function(input, output) {
  
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Number of Cases", 
      covid_data$total_cases[covid_data$location == input$country_select_tab1 & covid_data$date == input$date_tab1], 
      icon = icon("asterisk", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Number of Deaths", 
      covid_data$total_deaths[covid_data$location == input$country_select_tab1 & covid_data$date == input$date_tab1], 
      icon = icon("warning-sign", lib = "glyphicon"),
      color = "black"
    )
  })
  
  data <- reactive({
    covid_data %>% filter(location == input$country1 | location == input$country2, date >= input$daterange[1], date <= input$daterange[2])
  })
  output$tablo <- renderTable({
    brushedPoints(data(), input$click, yvar = input$variable)
  })
  output$graph <- renderPlot({
    ggplot(data(), aes(x = date, y = .data[[input$variable]], color = location)) + geom_point() + 
      scale_x_date(breaks = date_breaks("month")) + 
      theme(axis.text.x = element_text(angle = -90))
  })
  
  data_alone <- reactive({
    covid_data %>% filter(location == input$country_alone, date >= input$daterange_alone[1], date <= input$daterange_alone[2])
  })
  

  output$plot_alone <- renderPlot({
    req(input$variable_compare)
    p <- ggplot(data_alone(), aes(x = date))
    renk <- 0
    for (aa in 1:length(input$variable_compare)) {
      renk <- renk + 1
      p <- p + geom_point(aes(y = .data[[input$variable_compare[aa]]]), color =  renk)
    }
    p
  })
}

shinyApp(ui, server)


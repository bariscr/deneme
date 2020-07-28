library(shiny)
library(ggplot2)
library(dplyr)

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

ui <- fluidPage(
  h1("COVID COMPARISON"),
  selectInput("country1", "Select country 1", choices = unique(covid_data$location)),
  selectInput("country2", "Select country 2", choices = unique(covid_data$location)),
  selectInput("variable", "Select a variable to compare", 
              choices = c("total_cases", "new_cases","total_deaths", "new_deaths",
                          "total_cases_per_million", "new_cases_per_million",          
                          "total_deaths_per_million", "new_deaths_per_million")),
  dateRangeInput("daterange", "Select a date range", start = "2019-12-31", end = Sys.Date()),
  tableOutput("tablo"),
  mainPanel({
    plotOutput("graph")
  })
)

server <- function(input, output) {
  
  data <- reactive({
    covid_data %>% filter(location == input$country1 | location == input$country2, date >= input$daterange[1], date <= input$daterange[2])
  })
  output$tablo <- renderTable({
    head(data(), 5)
  })
  output$graph <- renderPlot({
    ggplot(data(), aes(x = date, y = .data[[input$variable]], color = location)) + geom_point()
  })
}

shinyApp(ui, server)

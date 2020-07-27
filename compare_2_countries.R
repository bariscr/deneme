library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  h1("COVID COMPARISON"),
  selectInput("country1", "Select country 1", choices = unique(covid_data_26072020$location)),
  selectInput("country2", "Select country 2", choices = unique(covid_data_26072020$location)),
  selectInput("variable", "Select a variable to compare", choices = names(covid_data_26072020)),
  dateRangeInput("daterange", "Select a date range", start = "2019-12-31", end = "2020-07-25"),
  tableOutput("tablo"),
  mainPanel({
    plotOutput("graph")
  })
)

server <- function(input, output) {
  data <- reactive({
    covid_data_26072020 %>% filter(location == input$country1 | location == input$country2, date >= input$daterange[1], date <= input$daterange[2])
  })
  output$tablo <- renderTable({
    head(data(), 5)
  })
  output$graph <- renderPlot({
    ggplot(data(), aes(x = date, y = .data[[input$variable]], color = location)) + geom_point()
  })
}

shinyApp(ui, server)

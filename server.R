shinyServer(function(input, output, session) {

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
)


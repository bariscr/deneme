shinyServer <- function(input, output, session) {
  
  output$approvalBox <- renderInfoBox({
    infoBox(
      title = "Number of Total Cases", 
      value = tags$p(covid_data$total_cases[covid_data$location == input$country_select_tab1 & covid_data$date == input$date_tab1], 
                     style = "font-size: 40px; color:white"),
      icon = icon("asterisk", lib = "glyphicon"),
      fill = TRUE,
      color = "yellow"
    )
  })

  output$approvalBox2 <- renderInfoBox({
    infoBox(
      "Number of Total Deaths", 
      value = tags$p(covid_data$total_deaths[covid_data$location == input$country_select_tab1 & covid_data$date == input$date_tab1], 
                     style = "font-size: 40px; color:white"),
      icon = icon("warning-sign", lib = "glyphicon"),
      fill = TRUE,
      color = "black"
    )
  })
  
  data_tab1 <- reactive({
    covid_data %>% filter(location == input$country_select_tab1, date == input$date_tab1)
  })
  output$approvalBox3 <- renderInfoBox({
    infoBox(
      "Death Rate (%)",
      value = tags$p(round(data_tab1()$total_deaths / data_tab1()$total_cases * 100, 1), style = "font-size: 40px; color: white"),
      icon = icon("battery-0"),
      fill = TRUE,
      color = "red"
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
      scale_y_continuous(breaks = seq(min(data()[input$variable]), max(data()[input$variable]), round(max(data()[input$variable])/10, 0))) + 
      scale_color_manual(name = "Countries", labels = sort(c(input$country1, input$country2)), values = c("green", "turquoise3")) + 
      labs(title = paste0("'",input$variable, "' Comparison between ", input$country1, " & ", input$country2), x = "Date") + 
      dark_theme_minimal() +
      theme(axis.text.x = element_text(angle = -45, color = "red4"),
            plot.title = element_text(face = "bold", size = 14, color = "red4", hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, color = "red4"),
            axis.text.y = element_text(size = 12, color = "red4"),
            ) 
  })
  
  data_alone <- reactive({
    covid_data %>% filter(location == input$country_alone, date >= input$daterange_alone[1], date <= input$daterange_alone[2])
  })
  
  
  output$plot_alone <- renderPlot({
    req(input$variable_compare)
    p <- ggplot(data_alone(), aes(x = date))
    renk <- 0
    for (aa in 1:length(input$variable_compare)) {
      renk <- renk + 2
      p <- p + geom_point(aes(y = .data[[input$variable_compare[aa]]]), color =  renk)
    }
    p <- p +  
      labs(title = paste0("Comparison between ", input$variable_compare[1], " - ", input$variable_compare[2], " variables in ", input$country_alone), x = "Date", y = "") +
      dark_theme_minimal() +
      theme(axis.text.x = element_text(angle = -45, color = "red4"),
            plot.title = element_text(face = "bold", size = 14, color = "red4", hjust = 0.5),
            axis.text.y = element_text(size = 12, color = "red4")
           ) 
    p
  })

  # TOP 10 sekmesinde kullanılacak veri setinin oluşturulması
  sirali <- reactive({
    sirali <- covid_data[, c("location", "date", input$top_10)] %>% filter(location != "World", date == input$top_10_date) 
    sirali <- arrange(sirali, desc(.data[[input$top_10]])) %>% head(10) 
    sirali <- sirali[, c(1, 3)]
  })
  
  # İlk 10 sıranın tablo gösterimi
  output$top_10_table <- renderTable({
    sirali()
  })
  
  # İlk 10 sıranın grafik gösterimi
  output$top_10_graph <- renderPlot({
    ggplot(sirali(), aes(x = reorder(location,  .data[[input$top_10]]), y = .data[[input$top_10]], fill = location)) +
      geom_bar(show.legend = FALSE,   stat = "identity") + coord_flip() + dark_theme_minimal() +
      labs(title = paste0("TOP 10 Countries (", input$top_10, ")"), x = "", y = "") + 
      theme(axis.text.x = element_text(angle = -45, color = "red4"),
            plot.title = element_text(face = "bold", size = 14, color = "red4", hjust = 0.5),
            axis.text.y = element_text(size = 12, color = "red4")
           ) 
  })
}



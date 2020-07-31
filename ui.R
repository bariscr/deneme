shinyUI <-   dashboardPage(
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
            fluidPage(
              selectInput("country_select_tab1", "Select Country", choices = unique(covid_data$location)),
              dateInput("date_tab1", "Select a Date", value = Sys.Date()),
              fluidRow(infoBoxOutput("approvalBox", width = 4),
                       infoBoxOutput("approvalBox2", width = 4),
                       infoBoxOutput("approvalBox3", width = 4))
              )
            ),    
    
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

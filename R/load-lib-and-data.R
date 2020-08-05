library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rio)
library(scales)
library(purrr)
library(bubblyr)
library(ggdark)

covid_data <- rio::import('https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.xlsx',
                          col_types = c("text", "text", "text", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", "numeric", 
                                        "text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", "numeric"))
covid_data$date <- as.Date(covid_data$date)

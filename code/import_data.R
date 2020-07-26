# import data ----
library(readxl)
covid_data_26072020 <- read_excel("covid_data.xlsx", 
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

# save data ----
save(covid_data_26072020, file = "covid_data_26072020.Rdata")

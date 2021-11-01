library(plyr)
library(weatherr)
library(tidyverse)
library(knitr)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)




gridpoint_forecast <- function(lat, long){
  
  latlong <- paste(lat, long, sep = ",")
  baseurl <- "https://api.weather.gov/points/"
  finalurl <- paste(baseurl,latlong, sep = "")
  html <- GET(finalurl)
  fr <- fromJSON(content(html, "text"), simplifyVector = TRUE)
  latlongurl <- fr$properties$forecast
  
  
  
  test <- GET(latlongurl)
  parsed <- fromJSON(content(test, "text"), simplifyVector = TRUE)
  table <- parsed$properties$periods %>% 
    bind_rows()
  table$timestamp <- Sys.Date()
  write.table(table, "C:\\Users\\Ryan\\Desktop\\weather model validation\\forecast.csv", append = T, sep = ",",col.names =  FALSE, row.names = FALSE )
  return(table)
  
}



gridpoint_forecast(35.5951, -82.5515)






# Scraping Rosebikes Mountainbike category

library(tidyverse)
library(rvest)
library(readr)

url <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"

html <- url %>% 
  read_html()

bike_name <- html %>% 
  html_nodes("h4.basic-headline__title") %>% 
  html_text()

price <- html %>% 
  html_nodes("div.catalog-category-bikes__price-title") %>% 
  html_text() %>% 
  parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))

data <- data.frame(bike_name, price)
print(data)
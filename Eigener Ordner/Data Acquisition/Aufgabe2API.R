#Step 1 Packages laden

library(rvest)


#Step 2 Daten "scrappen" (Entscheidung für Rennräder)

  url <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad"
  html <- url %>% 
  read_html()

  Modellbezeichnung <- html %>% 
  html_nodes("h4.basic-headline__title") %>% 
  html_text()

  Preis <- html %>% 
  html_nodes("div.catalog-category-bikes__price-title") %>% 
  html_text() %>% 
  parse_number(locale = locale(decimal_mark = ",", grouping_mark = "."))

#Step 3 Daten in lesbares Format überführen
  
Uebersichtstabelle <- data.frame(Modellbezeichnung, Preis)
print(Uebersichtstabelle)
    #Step 1 Packages laden
      library(httr)
      library(jsonlite)
      library(tidyverse)
      library(ggplot2)

    #Step 2 Anfrage stellen 
      # Wetter Vorhersage API 
      HFA <- GET("https://api.open-meteo.com/v1/forecast?latitude=53.54&longitude=9.84&hourly=true&windspeed_10m")

        # HFA = Wetterdaten an Station Finkenwerder Airport
        # windspeed_10m - Wind speed at 10 meters above ground in km/h
        # winddirection_10m - Wind direction at 10 meters above ground in °
        # temperature_2m:C - temperature at two meters in degree Celsius
      
      #Step 3 Daten für Windgeschwindigkeit auslesen

      windspeed_10m <- rawToChar(HFA$content) %>% fromJSON() 
      Zeit <- windspeed_10m[["hourly"]][1]
      Temperatur <- windspeed_10m[["hourly"]][2]
      
      #,winddirection_10m,hourly=temperature_2m
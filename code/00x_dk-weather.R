# created 8 april 2024
# purpose: learn to get weather data from DMI NOT WORKING
# author:  gina
# notes:


library(httr)
library(jsonlite)

rm(list = ls())


# climate data ------------------------------------------------------------

my_climate_api <- "7ae6e1b8-0c67-4604-a621-722d39ff1da0"

# #--you can test it using this url, didn't really work for me:  https://dmigw.govcloud.dk/v2/climateData?api-key=7ae6e1b8-0c67-4604-a621-722d39ff1da0
# 
# #--using this for R code: https://www.dataquest.io/blog/r-api-tutorial/

res <- GET("https://dmigw.govcloud.dk/v2/climateData")
res

#--status 403 means it isn't working, we want 200
#--need to feed it the api key somehow

#--use the notation from the DMI website:
headers = c('X-Gravitee-Api-Key' = my_climate_api)
res <- VERB("GET", url = "https://dmigw.govcloud.dk/v2/climateData", add_headers(headers))
res
#--it worked

#--convert to json-like format
data <- fromJSON(rawToChar(res$content))

names(data)

data$links

#try again, different site

res <- GET("https://dmigw.govcloud.dk/v2/climateData/bulk/stationValue/?api-key=7ae6e1b8-0c67-4604-a621-722d39ff1da0")

res

#--convert to json-like format
data <- fromJSON(rawToChar(res$content))

#--well shit I don't know what this means
names(data)
data$links

# meteorological data -----------------------------------------------------
#--this isn't what I want, but maybe I can get it to work and learn something?

my_weather_api <- "813ed608-ce2b-4972-89c4-c05520d41112"

headers = c('X-Gravitee-Api-Key' = my_weather_api)
res <- VERB("GET", url = "https://dmigw.govcloud.dk/v2/metObs/collections/station/items", add_headers(headers))
res

data <- fromJSON(rawToChar(res$content))

#--these names make more sense
names(data)

#--this looks more promising
tibble::as_tibble(data$features)

#--but it's not what I want...





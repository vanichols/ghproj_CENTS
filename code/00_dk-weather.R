# created 8/4/2024
# purpose: learn to get weather data from DMI
# notes:


library(tidyverse)

library(httr)
library(jsonlite)

rm(list = ls())


# climate data ------------------------------------------------------------

my_climate_api <- "7ae6e1b8-0c67-4604-a621-722d39ff1da0"

# #--you can test it using this url, didn't really work for me:  https://dmigw.govcloud.dk/v2/climateData?api-key=7ae6e1b8-0c67-4604-a621-722d39ff1da0
# 
# #--using this for R code: https://www.dataquest.io/blog/r-api-tutorial/
# #--this explains the process using python code: https://predictablysunny.com/posts/dmi_data/
# 
res <- GET("https://dmigw.govcloud.dk/v2/climateData")

#--status 403 means it isn't working, we want 200

#--use the notation from the DMI website:
headers = c('X-Gravitee-Api-Key' = my_climate_api)
res <- VERB("GET", url = "https://dmigw.govcloud.dk/v2/climateData", add_headers(headers))
res
#--it worked

#--convert to json-like format
data <- fromJSON(rawToChar(res$content))

names(data)

data$links

#try again

res <- GET("https://dmigw.govcloud.dk/v2/climateData/bulk/stationValue/?api-key=7ae6e1b8-0c67-4604-a621-722d39ff1da0")

res

#--convert to json-like format
data <- fromJSON(rawToChar(res$content))

names(data)

data$links

# meteorological data -----------------------------------------------------

my_weather_api <- "813ed608-ce2b-4972-89c4-c05520d41112"

headers = c('X-Gravitee-Api-Key' = my_weather_api)
res <- VERB("GET", url = "https://dmigw.govcloud.dk/v2/metObs/collections/station/items", add_headers(headers))
res

data <- fromJSON(rawToChar(res$content))

names(data)

as_tibble(data$features)


# download by hand --------------------------------------------------------


read_csv("data/raw/wea/2022-01-01.txt")


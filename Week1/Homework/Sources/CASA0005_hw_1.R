library(sf)
library(tidyverse)

#load the files
NZ_territory <- st_read("/Users/kangxinwei/Desktop/SDSV/0005/Week1/Homework/Raw/statsnzterritorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")
NZ_employ <- read_csv("/Users/kangxinwei/Desktop/SDSV/0005/Week1/Homework/Sources/employment_territory.csv", skip=1)
spec_csv("/Users/kangxinwei/Desktop/SDSV/0005/Week1/Homework/Sources/employment_territory.csv", skip=1) 

#plot geometry
NZ_territory %>% 
  st_geometry() %>%
  plot()

#join data
NZ_territory_employ <- NZ_territory%>%
  merge(.,
        NZ_employ,
        by.x="TA2018_V1_", 
        by.y="Area_Code")

#plot
library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
NZ_territory_employ %>%
  qtm(.,fill = "Employed Full time")

#export shape to a new GeoPackage
shape %>%
  st_write(.,"/Users/kangxinwei/Desktop/SDSV/0005/Week1/Homework/Sources/Rwk1.gpkg",
           "NZ_territory_employ",
           delete_layer=TRUE)

#connect to the .gpkg
library(readr)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),dbname="/Users/kangxinwei/Desktop/SDSV/0005/Week1/Homework/Sources/Rwk1.gpkg")

#examine what is in the .gpkg
con %>%
  dbListTables()

#Add your .csv and disconnect from the .gpkg:
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()
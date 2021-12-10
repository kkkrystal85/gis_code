library(rgdal)

#load data
library(sf)
shape <- st_read("/Users/kangxinwei/Desktop/SDSV/0005/Week1/prac1/prac1_data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

#have a quick look what the shapefile looks like
summary(shape)

#plots everything in the shapefile (all the attitbues)
plot(shape)

#the geometry (outline of the shape)
shape %>% 
  st_geometry() %>%
  plot()

library(tidyverse)
#load csv
mycsv <-  read_csv("/Users/kangxinwei/Desktop/SDSV/0005/Week1/prac1/prac1_data/fly_tipping_borough_edit.csv", skip=1)

mycsv

#join data
#matched our csv to our shape based on the GSS_CODE values in both
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", #shp的code
        by.y="Row Labels") #csv的code

shape%>%
  head(., n=10)

#make a quick thematic map (or a qtm) using the package tmap
library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2019-20")

tmap_mode("view")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2019-20")

#Export data
#把shape放进gpkg
shape %>%
  st_write(.,"/Users/kangxinwei/Desktop/SDSV/0005/Week1/prac1/prac1_data/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

#connect to gpkg
library(readr)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),dbname="/Users/kangxinwei/Desktop/SDSV/0005/Week1/prac1/prac1_data/Rwk1.gpkg")

#查看gpkg
con %>%
  dbListTables()

#在gpkg里add csv
con %>%
  dbWriteTable(.,
               "original_csv", #mycsv的新名字
               mycsv,
               overwrite=TRUE)

#查看gpkg
con %>%
  dbListTables()

#disconnect
con %>% 
  dbDisconnect()
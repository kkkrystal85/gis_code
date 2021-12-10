install.packages("here")
library(here)
here::here()

library(readr)
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"), #the encoding of the data (how it is stored)
                       na = "n/a") #清除里面的na 重要！！！！！！！
#重要！！！！清除n/a

#check what data type your new data set is
class(LondonData)

library(tidyverse)
#check that our data & wide dataset变long dataset
Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

#selest rows/london boroughs
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

#check
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()

#因为有两个重复的london，删除
LondonBoroughs<-LondonBoroughs %>%
  distinct()

#select columns
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 
LondonBoroughs_contains

#rename columns并clean
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()
LondonBoroughs

#Then selecting only the name of the Borough, mean life expectancy and normalised life expectancy, 
#arranging the output based on the normalised life expectancy in descending order…
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#将伦敦数据划分成两个组，一组高于英国平均身高，一组低于英国平均身高
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))

Life_expectancy2

#分成两组，看每组的range、数量、平均数
Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

#按ukdifference来分类，并统计number
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3

#plot
plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

install.packages("plotly")
library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

#spatial data in R
install.packages("maptools")
install.packages(c("classInt", "tmap"))

# might also need these ones
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)

here::here()
# shapefile in local folder
EW <- st_read(here::here("prac2_data",
                         "Local_Authority_Districts_(December_2015)_Boundaries",
                         "Local_Authority_Districts_(December_2015)_Boundaries.shp"))
EW

LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

#plot it using the qtm function
qtm(LondonMap)

LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", #EW里的code
        by.y="new_code", #LondonData里的code
        no.dups = TRUE)%>% #no duplications没有重复
  distinct(.,lad15cd, 
           .keep_all = TRUE) #refer to the column & keep all the columns

#left join
BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))

#create a choropleth map
library(tmap)
library(tmaptools)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015") #fill the map

#get a basemap we need to extract it from OpenStreetMap (OSM)
tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>%    #create a box(termed bounding box)
  tmaptools::read_osm(., type = "osm", zoom = NULL)  #basemap

tmap_mode("plot")

tm_shape(tmaplondon)+  #data from bounding box 
  tm_rgb()+  #raster aesthetic
  tm_shape(BoroughDataMap) +  #data from our London Borough layer
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015",   #it’s aesthetics(job seekers)
              style="jenks",  #how to divide the data into out colour breaks
              palette="YlOrBr",   #the colour scheme to use
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) +  #透明度
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))
tmap_mode("plot")

#Tidying data
flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)

#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )

# view the data
view(flytipping_long)

#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

widefly <- flytipping_long %>% 
  pivot_wider(
    names_from = year, 
    values_from = count)
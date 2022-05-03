library(stringr)
library(plyr)
library(readr)
library(data.table)
library(tidyverse)
library(readxl)
library(rnoaa)
library(dplyr)
library(sp)
library(sf)
library(gstat)

url = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
url2 = "https://www.ncei.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"

dest = "C:/Users/p7j5p/Documents/Paul's reports/spatial statistics/final project data/stations.csv"
dest2 = "C:/Users/p7j5p/Documents/Paul's reports/spatial statistics/final project data/inventory.csv"

download.file(url, dest)
download.file(url2, dest2)

#download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt" , destfile = "C:\\Users\\p7j5p\\Documents\\Paul's reports\\Data analysis\\final data")

#begin by downloading the full list of GHCND stations, then renaming the columns

stations <- read_table2("Paul's reports/spatial statistics/final project data/stations.csv")

inventory <- read_table2("inventory.csv")

stations$name <- str_c(stations$ST, " ", stations$JOHNS, " ", stations$COOLIDGE, " ", stations$FLD)

names(stations)[names(stations)=="ACW00011604"] <- "GHCND"

names(stations)[names(stations)=="ST JOHNS COOLIDGE FLD"] <- "name"

names(stations)[names(stations)=="17.1167"] <- "Lat"

names(stations)[names(stations)=="-61.7833"] <- "Long"

names(inventory)[names(inventory)=="ACW00011604"] <- "GHCND"
names(inventory)[names(inventory)=="17.1167"] <- "Lat"
names(inventory)[names(inventory)=="TMAX"] <- "Elem"
names(inventory)[names(inventory)=="-61.7833"] <- "Long"
names(inventory)[names(inventory)=="1949"] <- "First"
names(inventory)[names(inventory)=="1949_1"] <- "Last"

#Now filter down to the states for this analysis and take a peak

stations_states <- stations %>% filter(grepl("US1CO|US1ID|US1UT|US1WY", GHCND))

inventory_states <- inventory %>% filter(grepl("US1CO|US1ID|US1UT|US1WY", GHCND))

inventory_states1 <- inventory_states %>% filter(inventory_states$Elem == 'SNOW')

station_list <- select(inventory_states1, GHCND)

#glimpse(stations_states)

#write.csv(stations_states, file = "stations_states.csv")

#Selecting list to pull data for

#monitors <- select(stations_states, GHCND)

#testdata <- meteo_pull_monitors(monitors, keep_flags = FALSE, date_min = 2020-12-01,date_max = 2020-12-31, var = "PRCP")

#Quick test and we're off to the races

#stations_trim <- select(stations_states, GHCND)
#write.csv(stations_trim, file = "stations_trim.csv")

#This section of code originally came from http://spatialreasoning.com/wp/20170307_1244_r-reading-filtering-weather-data-from-the-global-historical-climatology-network-ghcnd
# and was written by Rao, M & Battaile, B.  I modified it slightly for this project

noaa_dir <- "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
noaaout <- "C:/Users/p7j5p/Documents/Paul's reports/spatial statistics/final project data/indv files"


numFiles <- length(station_list$GHCND)
dirname <- paste0(noaa_dir,"/ghcnd_all/")

dirname <- noaa_dir
options(download.file.method="libcurl")

for (i in 1:numFiles) {
  infile <- paste0(dirname, station_list$GHCND[i], ".dly")
  outfile <- paste0(noaaout, station_list$GHCND[i], ".csv")
  cols <- c( "A11", "I4", "I2", "A4",
             rep( c( "I5", "A1", "A1", "A1"), 31) )
  df <- read.fortran(infile, cols, na.strings="-9999") # -9999 indicates missing data
  
  # next, fill in the column names
  tmp <- c("Val","xxM","xxQ","xxS") # xx so we can ditch them later
  vhdrs <- paste(   rep(tmp,31),   rep(1:31,each=4), sep="")
  hdrs <- c("ID", "year", "month", "element", vhdrs)
  names(df) <- hdrs
  df <- df[df$year >= 1995 & df$year <= 2020,]
  df_out <- dplyr::select(df, -matches("xx*")) # get rid of M, Q, S 
  write.csv(df_out, outfile)
}

file_list <- list.files("C:/Users/p7j5p/Documents/Paul's reports/spatial statistics/final project data")
#the wd needs to be set to the folder containing the files for this to run
dataset3 <- ldply(list.files(), read.csv, header=TRUE)

write.csv(dataset3, "Combined_data.csv")

Combined_data1 <- Combined_data %>% filter(!(month %in% (5:12)))

Combined_data1 <- Combined_data1 %>% filter(element == 'SNOW')

write.csv(Combined_data1, 'snow_data.csv')

stations <- read_sf("stations.geojson")
names(stations)[names(stations)=='Amt..mm.'] <- 'Amt'

stations1 <- as_Spatial(stations)
stations.grid <- makegrid(stations1, cellsize = 5000)
stations.grid <- as_Spatial(st_as_sf(stations.grid, coords = c("x1", "x2"), crs = 6933, agr = "constant"))
plot(stations.grid)


stations.vgm <- variogram(Amt~1, stations1)
stations.fit <- fit.variogram(stations.vgm, model=vgm(1, "Lin", 2500, 1))

library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(rgdal)
library(dplyr)
library(plotly)
library(mapproj)

set.seed(8000)

#Read data from shapefile
my_spdf <- readOGR(dsn="C:/Users/Danie/Desktop/WCW19", layer="Wards__December_2019__Boundaries_GB_BGC")

results <- read.csv("C:/Users/Danie/Desktop/WiltshireCouncilResultsSalisbury2017.csv")
#names(results)

#Convert shapefile to datatable
spdf_fort <- fortify(my_spdf, region="wd19nm")

#Merge results and shapefile by ID
merged <-merge(spdf_fort,results, by="id")

final.data<-merged[order(merged$order), ]

#Set colours for parties

cols <- c(
  "Lab" = "#e91e0f",
  "C" = "#0676c9",
  "Grn" = "green",
  "LD" = "#efac19",
  "UKIP" = "purple",
  "TWCR" = "cyan",
  "Ind" = "grey"
)

p <- ggplot(data=final.data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = winning_party),
               colour = alpha("white", 1/2), size = 0.05)+
  ggtitle("Winning Party in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_manual(name="id", values = cols) +
  theme_void()

fig <- ggplotly(p)

fig

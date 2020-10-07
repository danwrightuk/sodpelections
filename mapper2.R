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
library(viridis)
library(broom)

set.seed(420)

#Read data from shapefile
my_spdf <- readOGR(dsn="C:/Users/Danie/Desktop/WCW19", layer="Wards__December_2019__Boundaries_GB_BGC")
WM_spdf <- readOGR(dsn="C:/Users/Danie/Desktop/SalisburyWestminsterBoundaries", layer="Westminster_Parliamentary_Constituencies__December_2019__Boundaries_UK_BFC")

results <- read.csv("C:/Users/Danie/Desktop/percentvoteWC2017.csv")

#Convert shapefile to datatable
spdf_fort <- fortify(my_spdf, region="wd19nm")

#Merge results and shapefile by ID
merged <-merge(spdf_fort,results, by="id")

final.data<-merged[order(merged$order), ]

#Average position for labelling maps
label.data <- final.data %>%
  group_by(id) %>%
  summarise(long = mean(long), lat = mean(lat))

#Set colours for parties (using BBC election coverage values)
cols <- c(
  "Lab" = "#e91e0f",
  "C" = "#0676c9",
  "Grn" = "#5fb25f",
  "LD" = "#efac19",
  "UKIP" = "#712f87",
  "TWCR" = "cyan",
  "Ind" = "#bababa"
)

p <- ggplot(data=final.data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = winparty),
               colour = alpha("black", 1/2), size = 0.3)+
  theme_void()+
  ggtitle("Winning Party in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_manual(name="Party", values = cols) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    
    legend.position = c(0.7, 0.09)
  ) +
  coord_map() #Adds Mercator Projection

fig <- ggplotly(p)

fig

q <- ggplot(data=final.data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = TWCRvote),
               colour = alpha("black", 1/2), size = 0.3)+
  theme_void()+
  labs(
    title = "TWCR Vote in 2017 Wiltshire Council Election",
    subtitle = "",
    caption = "Dan Wright | SODP"
  ) +
  scale_fill_distiller(palette = "Greys" , na.value="white", direction=1, n=9, name="TWCR vote /%", guide = guide_colorbar( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm")) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig2 <- ggplotly(q)
fig2

#myMap <- get_stamenmap(bbox=c(left = -1.9034, bottom =51.0274, right=-1.6768, top =51.1225), maptype = "toner-lite", colour="bw", crop=FALSE)
#ggmap(myMap)
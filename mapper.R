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


label.data <- final.data %>%
  group_by(id) %>%
  summarise(long = mean(long), lat = mean(lat))

#Set colours for parties

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
  geom_polygon(aes(fill = winning_party),
               colour = alpha("white", 1/2), size = 0.05)+
  ggtitle("Winning Party in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_manual(name="id", values = cols) +
  theme(legend.title=element_blank()) +
  coord_map() #Adds Mercator Projection

fig <- ggplotly(p)

fig

filtered <- dplyr::filter(final.data, grepl('Lab', party))

q <- ggplot(data=filtered, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percentvote),
               colour = alpha("white", 1/2), size = 0.05)+
  ggtitle("Labour Vote in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_distiller(palette = "Reds" , direction=1, n=9, name="Labour vote", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig2 <- ggplotly(q)

fig2

filtered2 <- dplyr::filter(final.data, grepl('LD', party))

ld <- ggplot(data=filtered2, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percentvote),
               colour = alpha("white", 1/2), size = 0.05)+
  ggtitle("Lib Dem Vote in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_distiller(palette = "Oranges" , direction=1, n=9, name="Lib Dem vote", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig3 <- ggplotly(ld)

fig3

filtered3 <- dplyr::filter(final.data, grepl('C', party))

consvote <- ggplot(data=filtered3, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percentvote),
               colour = alpha("white", 1/2), size = 0.05)+
  ggtitle("Conservative Vote in Wiltshire Council Elections in Salisbury, 2017") +
  scale_fill_distiller(palette = "Blues" , direction=1, n=9, name="Conservative vote", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig4 <- ggplotly(consvote)

fig4

filteredgreen <- dplyr::filter(final.data, grepl('Grn', party))
filteredgreen$id[ is.na(filteredgreen$id)] = 0.001

greenvote <-
  ggplot() +
  geom_polygon(data = filteredgreen, aes(fill = percentvote, x = long, y = lat, group = group), color = "Black", size = 0.3, alpha=0.9) +
  theme_void() +
  scale_fill_distiller(palette = "Greens" , na.value="white", direction=1, breaks = pretty_breaks(n = 9), name="% votes for Green Party", guide = guide_colorbar( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), title.position = 'top', nrow=1) ) +
  labs(
    title = "Green Vote in Wiltshire Council Elections in Salisbury, 2017",
    subtitle = "% of Green Party Votes by Ward",
    caption = "Data: SODP | Creation: Dan Wright | sodp.org.uk"
  )   +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
  
  )  +
  coord_map() +
  geom_text(data=label.data, aes(long, lat, label = id), size=3)

greenvote

fig5 <- ggplotly(greenvote)

fig5

fig6 <- fig5 %>%
  layout(title = list(text = paste0('Green Vote in Wiltshire Council Elections in Salisbury, 2017',
                                    '<br>',
                                    '<sup>',
                                    '% of Green Party Votes by Ward',
                                    '</sup>')))

fig6

library(openxlsx)
library(sf)
library(mapview)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
library(ggpubr)
library(dplyr)
library(openxlsx)

index.itu<- read.xlsx("data_maps.xlsx")
colnames(index.itu)


count <- index.itu %>% 
  filter(ITU >= 0 & ITU < 0.2) %>% 
  nrow()



#### 21 with values between 0 e 0.2
#### 35 with values between 0.2 e 0.4
#### 21 with values between 0.4 e 0.6
#### 3 with values between 0.6 e 0.8
#### 5 with values between 0.8 e 1

#_______________________________________________________________________________


#############global map
world <- ne_countries(scale = "medium", returnclass = "sf")

#######_________________________________________________________________________

  map1.cen <- ggplot() +
    geom_sf(data = world, fill = NA) +
    geom_point(data = index.itu, aes(x = Longitude, y = Latitude, color = ITU_intervals, size = ITU_intervals), shape = 16, show.legend = T) +  # Map the size_variable to the size aesthetic
    coord_sf(xlim = c(-116.8764, -31.87643), ylim = c(-40.3604, 35.27248), expand = FALSE) +
    theme_test(base_size = 7) +
    ggplot2::theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "mm"),
                   legend.text = element_text(size = 7), 
                   axis.text = element_text(size = 7),
                   axis.title.x = element_text(size = 7),
                   axis.title.y = element_text(size = 7),
                   legend.key.height = unit(0.35, 'cm'),
                   legend.key.width = unit(0.35, 'cm'),
                   legend.position = c(0.2, .4),
                   legend.key.size = unit(0.5, "lines"))+
    labs(x = "Longitude", y = "Latitude") +
    scale_size_continuous(range = c(1, 5)) +
    scale_color_viridis_c(option = "D", breaks=c(0.2, 0.4, 0.6, 0.8, 1))+
    guides(size = "none") +
    labs(color = "Taxonomic 
uncertainty (ITU)")

map1.cen



#######_________________________________________________________________________

  map2.cen <- ggplot() +
    geom_sf(data = world, fill = NA) +
    geom_point(data = index.itu, aes(x = Longitude, y = Latitude, color = DR_intervals, size = DR_intervals), shape = 16, show.legend = T) +  # Map the size_variable to the size aesthetic
    coord_sf(xlim = c(-116.8764, -31.87643), ylim = c(-40.3604, 35.27248), expand = FALSE) +
    theme_test(base_size = 7) +
    ggplot2::theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "mm"),
                   legend.text = element_text(size = 7), 
                   axis.text = element_text(size = 7),
                   axis.title.x = element_text(size = 7),
                   axis.title.y = element_text(size = 7),
                   legend.key.height = unit(0.35, 'cm'),
                   legend.key.width = unit(0.35, 'cm'),
                   legend.position = c(0.2, .4),
                   legend.key.size = unit(0.5, "lines"))+
    labs(x = "Longitude", y = "Latitude") +
    scale_size_continuous(range = c(1, 5)) +
    scale_color_viridis_c(option = "D", breaks=c(0.2, 0.4, 0.6, 0.8, 1))+
    guides(size = "none") +
    labs(color = "Speciation 
rate (DR)")
map2.cen




#_______________________________________________________________________________

{tiff("Figure.maps.tiff", width = 16.9, height = 6, res = 600, units = "cm")
  
figure.2ch <- ggarrange(map1.cen, map2.cen, ncol = 2, labels = c("(a)", "(b)"),
                          font.label = list(size = 10, face = "plain", color ="black"))}
figure.2ch

dev.off()













rm(list = ls())

setwd("~/Desktop/PhD/Publications/Li Chong et al. ocean temp variability")
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(dplyr)
library(tidyverse)

metadata = read.csv("metadata.csv")

#filter by depth
metadata = metadata[which(metadata$depth_in_m <= 12.5), ]
metadata = metadata[which(metadata$depth_in_m != -999.0), ]
metadata = metadata[which(metadata$climate_classification != "Polar"), ]

#group study ids of similar freq
metadata <- metadata %>% 
  mutate(plot_id = gsub( " .*$", "", metadata$plot_id))
metadata = metadata[match(unique(metadata$plot_id), metadata$plot_id),]
#

#remove different depths at same coordinate
metadata2 <- metadata %>% 
  mutate(plot_id = gsub( "\\..*", "", metadata$plot_id))
metadata2 = metadata2[match(unique(metadata2$plot_id), metadata2$plot_id),]
#

#reclassify climate classification
metadata2 <- metadata2 %>% 
  mutate(climate_classification = ifelse(abs(metadata2$lat_in_degrees) < 20, "Tropical", 
                                         ifelse(abs(metadata2$lat_in_degrees) > 35, "Temperate", "Subtropical")))
#check = metadata2[,c(5,28)]

metadata2$climate_classification <- factor(metadata2$climate_classification , 
                                           levels=c("Tropical", "Subtropical", "Temperate"))

# # try to remove PMEL data
# metadata2 = metadata2[-which(metadata2$data_source == "Pacific Marine Environmental Laboratory"),]
# #

world <- map_data("world")

mp <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "lightgray", fill = "lightgray", size = 0.1
  ) + theme_bw() 


MAP<-mp + geom_point(data = metadata2, 
                aes(x = long_in_degrees, y = lat_in_degrees, 
                    color = climate_classification), size = 3.5, alpha = 0.6)+theme_void(base_size=13) +
  labs(color="Climate classification ") + 
  scale_size(guide = "none") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
                                        legend.position = c(0.13, 0.18), #positions legend within plot at x and y
        legend.spacing.y = unit(0.1, "mm"),legend.title.align=0,
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+ # adds box around legend and fixes spacing 
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC"))+ 
  scale_y_continuous(expand = c(0,0), limits = c(-60, 90))+scale_x_continuous(expand = c(0,0), limits = c(-180, 190)) +
  annotate('text',x=-82.58167, y = -0.871389 , label = 'a', size =7,fontface='bold')+
  annotate('text',x=55 , y = -12 , label = 'b', size =7,fontface='bold')+
  annotate('text',x=-48.5451 , y = -27.9354 , label = 'c', size =7,fontface='bold')+
  annotate('text',x=115.4616 , y = -20.9019 , label = 'd', size =7,fontface='bold')+
  annotate('text',x=148.0925 , y = -42.1239 , label = 'e', size =7,fontface='bold')+
  annotate('text',x=-72.30889 , y = -41.49556 , label = 'f', size =7,fontface='bold')


MAP

# this saves fig as high-quality pdf
pdf(width = 8, useDingbats=TRUE,height = 3.5, bg="white", file="World_Map") 
MAP
dev.off()

ggsave(filename="Fig1_JMS.png",height=6, width=10, plot=MAP, device="png")


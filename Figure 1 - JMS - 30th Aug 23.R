########## COMBINED FIGURE (MAP + TIME-SERIES) ##########

#########################
### Part 1: World Map ###
#########################

rm(list = ls())

setwd("C:/Users/dlcyli/OneDrive/Ocean_temperature_variability_2022")
#setwd("~/Documents/GitHub/Ocean_temperature_variability_2022/")
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(dplyr)
library(tidyverse)

theme_JMS <- function () {
  theme_bw(base_size = 12) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 12, face = "plain"),
    legend.title = element_text(size = 14, face = "bold")
  )
}

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

### Recenter ####

center <- 200 # positive values only - US centered view is 260

# shift coordinates to recenter loggers
metadata2$long.recenter <-  ifelse(metadata2$long_in_degrees < center - 180 , 
                                   metadata2$long_in_degrees + 360, metadata2$long_in_degrees) 

# shift coordinates to recenter world
world <- map_data("world")
world$long.recenter <-  ifelse(world$long  < center - 180 , world$long + 360, world$long)

### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}

library(plyr)
# now regroup
worldmap.rg <- ddply(world, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order")  # use the new grouping var

# plot
mp <- ggplot() +
  geom_polygon(aes(long.recenter,lat,group=group.regroup), size = 0.1, 
               fill="lightgray", colour = "lightgray", data=worldmap.cp) + theme_bw() 

MAP<-mp + geom_point(data = metadata2, 
                     aes(x = long.recenter, y = lat_in_degrees, 
                         color = climate_classification), size = 3.5, alpha = 0.6)+theme_JMS() +
  labs(color="Climate classification ") + 
  scale_size(guide = "none") + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = c(0.13, 0.18), #positions legend within plot at x and y
        legend.spacing.y = unit(0.1, "mm"),legend.title.align=0,
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())+ # adds box around legend and fixes spacing 
  scale_color_manual(values=c("#FC440F", "#E8C547", "#50D8D7"))+ 
  scale_y_continuous(expand = c(0,0), limits = c(-60, 90))+
  scale_x_continuous(expand = c(0,0), limits = c(-180, 190)) +
  annotate('text',x=-82.58167, y = -0.871389 , label = 'a', size =7,fontface='bold')+
  annotate('text',x=55 , y = -12 , label = 'b', size =7,fontface='bold')+
  annotate('text',x=-48.5451 , y = -27.9354 , label = 'c', size =7,fontface='bold')+
  annotate('text',x=115.4616 , y = -20.9019 , label = 'd', size =7,fontface='bold')+
  annotate('text',x=148.0925 , y = -42.1239 , label = 'e', size =7,fontface='bold')+
  annotate('text',x=-72.30889 , y = -41.49556 , label = 'f', size =7,fontface='bold')


MAP

LatRange <- ggplot(data=metadata2, aes(y=lat_in_degrees,x=0,color=climate_classification),size=2.5, alpha=0.6)+
  theme_classic(base_size=13)+geom_point()+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        legend.position = 'none', #positions legend within plot at x and y
        legend.spacing.y = unit(0.1, "mm"),legend.title.align=0,
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = margin(0.2,1.5,0.2,0.2, "cm"))+
  scale_color_manual(values=c("#FC440F", "#E8C547", "#50D8D7"))+ ylab("Latitudinal Range (°)")+
  scale_y_continuous(expand = c(0,0), limits = c(-60, 90),position = "right")+scale_x_continuous(expand = c(0,0), limits = c(-1, 1))


LatRange

#################################
### Part 2: Time-series plots ###
#################################

# rm(list = ls())
# setwd("~/Documents/GitHub/Ocean_temperature_variability_2022/")

library(ggplot2)
library(measurements)
library(lubridate)
library(dplyr)
library(RMySQL)
library(ggpubr)
library(geosphere)
library(gridExtra)
library(grid)


### TROPICAL 
plot_id = "98"

df = read.csv("plot_id_98.csv")

freq = 15

max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time1 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

#default font is 11 when using text()
assign(paste0("plot1"), ggplot(df, aes(x = time1, y = temp)) + geom_line(colour='#FC440F') + theme_classic() + 
         labs(title = "Ecuador (0.87°S, 15 mins)") + geom_hline(yintercept=28.766,
                                                       linetype='dashed')+ 
         geom_hline(yintercept=15.031,linetype='dashed') +
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + 
         annotation_custom(grobTree(textGrob("a", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 2.62 °C & Monthly median: 7.65 °C", x=0.55,  y=0.12,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.34,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.8,
                                             gp=gpar(fontsize=10)))))

plot1 = plot1 + coord_cartesian(ylim = c(4,35))
plot1
#
plot_id = "380"

df = read.csv("plot_id_380.csv")

freq = 10

max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time2 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

assign(paste0("plot2"), ggplot(df, aes(x = time2, y = temp)) + geom_line(colour='#FC440F') + theme_classic() + 
         labs(title = "Indian Ocean (12.00°S, 10 mins)") + geom_hline(yintercept=32.377,
                                                             linetype='dashed')+ 
         geom_hline(yintercept=24.639,linetype='dashed')+
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               text = element_text(size=12)) + 
         annotation_custom(grobTree(textGrob("b", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 0.38 °C & Monthly median: 2.07 °C", x=0.55,  y=0.12,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.62,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.91,
                                             gp=gpar(fontsize=10))))) 

plot2 = plot2 + coord_cartesian(ylim = c(4,35))
plot2

#### SUBTROPICAL
plot_id = "454"

df = read.csv("plot_id_454.csv")

freq = 20

min(df$temp, na.rm = TRUE)
max(df$temp, na.rm = TRUE)
max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time3 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

assign(paste0("plot3"), ggplot(df, aes(x = time3, y = temp)) + geom_line(colour='#E8C547') + theme_classic() + 
         labs(title = "Brazil (27.94°S, 20 mins)") + geom_hline(yintercept=25.416,
                                                       linetype='dashed')+ 
         geom_hline(yintercept=14.517,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + 
         annotation_custom(grobTree(textGrob("c", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 0.67 °C & Monthly median: 4.76 °C", x=0.55,  y=0.12,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.32,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.71,
                                             gp=gpar(fontsize=10))))) 

plot3 = plot3 + coord_cartesian(ylim = c(4,35))
plot3
#
plot_id = "286"

df = read.csv("plot_id_286.csv")

freq = 10

max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time4 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

assign(paste0("plot4"), ggplot(df, aes(x = time4, y = temp)) + geom_line(colour='#E8C547') + theme_classic() + 
         labs(title = "Western Australia (20.90°S, 10 mins)") + geom_hline(yintercept=31.3002,
                                                                  linetype='dashed')+ 
         geom_hline(yintercept=20.8347,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + 
         annotation_custom(grobTree(textGrob("d", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 0.60 °C & Monthly median: 2.89 °C", x=0.55,  y=0.12,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.51,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.88,
                                             gp=gpar(fontsize=10))))) 

plot4 = plot4 + coord_cartesian(ylim = c(4,35))
plot4

#### TEMPERATE
plot_id = "65 - (b)"

df = read.csv("plot_id_65b.csv")

freq = 60

max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time5 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

assign(paste0("plot5"), ggplot(df, aes(x = time5, y = temp)) + geom_line(colour="#50D8D7") + theme_classic() + 
         labs(title = "Tasmania (43.23°S, 1 hr)") + geom_hline(yintercept=20.793,
                                                         linetype='dashed')+ 
         geom_hline(yintercept=8.99,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.y = element_blank(),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + xlab("Year") + 
         annotation_custom(grobTree(textGrob("e", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 0.34 °C & Monthly median: 2.60 °C", x=0.55,  y=0.92,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.16,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.575,
                                             gp=gpar(fontsize=10))))) 

plot5 = plot5 + coord_cartesian(ylim = c(4,35))
plot5
#

plot_id = "413"

df = read.csv("plot_id_413.csv")

freq = 60

max(df$temp, na.rm = TRUE) - min(df$temp, na.rm = TRUE)

x = as.POSIXct(df$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
y = x + 60*length(df$temp)*freq - 60*freq

time6 = seq(from = as.POSIXct(df$measurement_date[1], tz = "GMT"), 
            to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))

assign(paste0("plot6"), ggplot(df, aes(x = as.Date(time6), y = temp)) + geom_line(colour="#50D8D7") + theme_classic() + 
         labs(title = "Chile (41.50°S, 1 hr)") + geom_hline(yintercept=21.71,
                                                      linetype='dashed')+ 
         geom_hline(yintercept=6.54,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.y = element_blank(),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + xlab("Year")+ scale_x_date(date_breaks='3 month',date_labels = "%b %Y") + 
         annotation_custom(grobTree(textGrob("f", x=0.08,  y=0.91,
                                             gp=gpar(fontsize=15,fontface='bold')))) + 
         annotation_custom(grobTree(textGrob("Daily median: 1.58 °C & Monthly median: 6.17 °C", x=0.55,  y=0.92,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.09,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.6,
                                             gp=gpar(fontsize=10))))) 


plot6 = plot6 + coord_cartesian(ylim = c(4,35))
plot6
#

#####################################################
######### COMBINE ALL PLOTS INTO ONE FIGURE #########
#####################################################
(layout_matrix <- matrix(c(1, 2, 3,4,4, 5, 6,7, 8), nrow = 3, byrow = TRUE))
blankPlot<-ggplot() + geom_blank()+theme_void()

layout <- rbind(c(1, 2,3),
                c(4,4,4),
                c(5,6,7))

grid.arrange(plot1, plot2,plot3,
             arrangeGrob(blankPlot,MAP,LatRange,widths=c(0.08,0.8,0.12)),
             plot4, plot5, plot6, ncol = 3, nrow=3, layout_matrix = layout,heights=c(1,2,1))



# Save final fig: 
#setwd("~/Desktop/PhD/Publications/Li Chong et al. ocean temp variability")
ggsave(filename="Fig1_Aug2023_JMS.png",height=9, width=12.5, plot=grid.arrange(plot1, plot2,plot3,
                                                                            arrangeGrob(blankPlot,MAP,LatRange,widths=c(0.08,0.8,0.12)),
                                                                            plot4, plot5, plot6, ncol = 3, nrow=3, layout_matrix = layout,heights=c(1,2,1)), device="png")

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
metadata = metadata[which(metadata$measurement_freq_in_mins <= 60), ]
metadata = metadata[which(metadata$duration_in_years >= 0.5), ]

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

metadata = read.csv("metadata.csv") # re-read metadata because you modified it earlier
additional_ts = read.csv("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Additional time series for supps as requested by Julia Baum.csv")
additional_ts = additional_ts[1:6,1:3]

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
        legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())+ # adds box around legend and fixes spacing 
  scale_color_manual(values=c("#FC440F", "#E8C547", "#50D8D7"))+ 
  scale_y_continuous(expand = c(0,0), limits = c(-60, 90))+
  scale_x_continuous(expand = c(0,0), limits = c(20, 380)) +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[1])] < center - 180 , 
                           metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[1])] + 360, 
                           metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[1])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[1])], label = 'a', size =7,fontface='bold') +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[2])] < center - 180 , 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[2])] + 360, 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[2])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[2])], label = 'b', size =7,fontface='bold') +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[3])] < center - 180 , 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[3])] + 360, 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[3])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[3])], label = 'c', size =7,fontface='bold') +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[4])] < center - 180 , 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[4])] + 360, 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[4])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[4])], label = 'd', size =7,fontface='bold') +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[5])] < center - 180 , 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[5])] + 360, 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[5])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[5])], label = 'e', size =7,fontface='bold') +
  annotate('text',
           x=ifelse(metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[6])] < center - 180 , 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[6])] + 360, 
                    metadata$long_in_degrees[which(metadata$plot_id == additional_ts$Temperate[6])]), 
           y = metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[6])], label = 'f', size =7,fontface='bold')


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
library(ggplot2)
library(measurements)
library(lubridate)
library(dplyr)
library(RMySQL)
library(ggpubr)
library(geosphere)
library(gridExtra)
library(grid)

mydb = dbConnect(MySQL(), user='root', password='password', dbname='hots', host='localhost')

#This is needed to avoid error "loading local data is disabled this must be enabled on both the client and server sides "
dbSendQuery(mydb, "SET GLOBAL local_infile = true;")

max_ylim = -99
min_ylim = 99
for (j in 1:length(additional_ts$Temperate)) {
  plot_idd = additional_ts$Temperate[j]
  tablee = metadata$tablee[which(metadata$plot_id == plot_idd)]
  if (substr(plot_idd, nchar(plot_idd), nchar(plot_idd)) == "'") {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_idd, "'';"))
  } else {
    data = dbSendQuery(mydb, paste0("SELECT plot_id, measurement_date, oxy_conc, ph, salinity, temp 
                                FROM ", tablee, " WHERE plot_id = '", plot_idd, "';"))
  }
  
  data = fetch(data, n=-1)
  if (length(data$plot_id) > 0) {
    freq = metadata$measurement_freq_in_mins[which(metadata$plot_id == plot_idd)]
    
    if (j == 1) {
      index_start = 1
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    } else if (j == 2) {
      index_start = 1 
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    } else if (j == 3) {
      index_start = 1
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    } else if (j == 4) {
      index_start = 1
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    } else if (j == 5) {
      index_start = 1 
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    } else if (j == 6) {
      index_start = 1 
      index_end = index_start + 365*24*(60/freq)
      data = data[index_start:index_end,]
    }
    
    x = as.POSIXct(data$measurement_date[1], format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    y = x + 60*length(data$temp)*freq - 60*freq
    
    time = seq(from = as.POSIXct(data$measurement_date[1], tz = "GMT"), 
                to = as.POSIXct(y, tz = "GMT"), by = paste(freq, "mins"))
    data$new_date = time
    if (max(data$temp, na.rm = TRUE) > max_ylim) {
      max_ylim = max(data$temp, na.rm = TRUE)
    }
    if (min(data$temp, na.rm = TRUE) < min_ylim) {
      min_ylim = min(data$temp, na.rm = TRUE)
    }
    assign(paste0("data",j), data)
    assign(paste0("max_temp",j), max(data$temp, na.rm = TRUE))
    assign(paste0("min_temp",j), min(data$temp, na.rm = TRUE))
    #default font is 11 when using text()
    assign(paste0("plot_",j), ggplot(data = get(paste0("data",j)), aes(x = new_date, y = temp)) + 
             # VERY IMPORTANT: to make it dynamic, everything in aes() must be from 'data'
             # Else, will get error:
             # Aesthetics must be either length 1 or the same as the data
             geom_line(color='#50D8D7') + theme_classic() + 
             labs(title = paste0(metadata$loc_name[which(metadata$plot_id == additional_ts$Temperate[j])],
                                 " (", ifelse(substr(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])],
                                                     1,2) == "'-", 
                                              paste0(as.character(format(round(as.double(substr(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])],
                                                                                      3,nchar(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])]))), 2), nsmall = 2)), "°S"),
                                              ifelse(substr(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])],
                                                            1,1) == "-", 
                                                     paste0(as.character(format(round(as.double(substr(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])],
                                                                                             2,nchar(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])]))), 2), nsmall = 2)), "°S"),
                                                     paste0(as.character(format(round(as.double(metadata$lat_in_degrees[which(metadata$plot_id == additional_ts$Temperate[j])], 2)), nsmall = 2)), "°N"))),
                                 ", ", freq, " mins)"
                                 )) + 
             geom_hline(yintercept=get(paste0("max_temp",j)),
                        linetype='dashed') + 
             geom_hline(yintercept=get(paste0("min_temp",j)),
                        linetype='dashed') +
             theme(plot.title = element_text(hjust = 0.5),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   axis.title.x = element_blank(),
                   text = element_text(size=9)) + ylab("Temperature (°C)") + 
             annotation_custom(grobTree(textGrob(letters[j], x=0.08,  y=0.92,
                                                 gp=gpar(fontsize=15,fontface='bold')))))
  }
}

setwd("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")
temp_range = read.csv("temp range - 19 Mar 2023_with_spatial_blocks.csv")
# a few more changes to the plots 
for (j in 1:length(additional_ts$Temperate)) {
  plot_idd = additional_ts$Temperate[j]
  
  if (j == 1) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    y1 = 0.1
    y2 = 0.53
    y3 = 0.88
  } else if (j == 2) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == "65")[1]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == "65")[1]], 2), nsmall = 2)
    y1 = 0.1
    y2 = 0.34
    y3 = 0.67
  } else if (j == 3) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    y1 = 0.1
    y2 = 0.27
    y3 = 0.54
  } else if (j == 4) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    y1 = 0.1
    y2 = 0.3
    y3 = 0.69
  } else if (j == 5) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == additional_ts$Temperate[j])[1]], 2), nsmall = 2)
    y1 = 0.92
    y2 = 0.12
    y3 = 0.55
  } else if (j == 6) {
    daily_median = format(round(temp_range$daily_median[which(temp_range$plot_id == "32")[2]], 2), nsmall = 2)
    monthly_median = format(round(temp_range$monthly_median[which(temp_range$plot_id == "32")[2]], 2), nsmall = 2)
    y1 = 0.1
    y2 = 0.32
    y3 = 0.62
  }
  
  assign(paste0("plot_",j), get(paste0("plot_",j)) +
           coord_cartesian(ylim = c(min_ylim-5,max_ylim+5))+
           annotation_custom(grobTree(
             textGrob(paste0("Daily median: ", daily_median, 
                             " °C & Monthly median: ", monthly_median," °C"),
                      x=0.55,  y=y1,
                      gp=gpar(fontsize=10)))) + 
           annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=y2,
                                               gp=gpar(fontsize=10)))) + 
           annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=y3,
                                               gp=gpar(fontsize=10)))))
  
}

plot_1
plot_2
plot_3
plot_4
plot_5
plot_6

#####################################################
######### COMBINE ALL PLOTS INTO ONE FIGURE #########
#####################################################
(layout_matrix <- matrix(c(1, 2, 3,4,4, 5, 6,7, 8), nrow = 3, byrow = TRUE))
blankPlot<-ggplot() + geom_blank()+theme_void()

layout <- rbind(c(1, 2,3),
                c(4,4,4),
                c(5,6,7))

grid.arrange(plot_1, plot_2,plot_3,
             arrangeGrob(blankPlot,MAP,LatRange,widths=c(0.08,0.8,0.12)),
             plot_4, plot_5, plot_6, ncol = 3, nrow=3, layout_matrix = layout,heights=c(1,2,1))



# Save final fig: 
#setwd("~/Desktop/PhD/Publications/Li Chong et al. ocean temp variability")
ggsave(filename="FigS7.png",height=9, width=12.5, plot=grid.arrange(plot_1, plot_2,plot_3,
                                                                            arrangeGrob(blankPlot,MAP,LatRange,widths=c(0.08,0.8,0.12)),
                                                                            plot_4, plot_5, plot_6, ncol = 3, nrow=3, layout_matrix = layout,heights=c(1,2,1)), device="png")

rm(list = ls())

#setwd("C:/Users/Owner/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")
setwd("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/")

library(ggplot2)
library(dplyr)
# library(visreg)
library(gridExtra)
library(grid)
# library(showtext)
# library(emojifont)
library(scales) # to specify number of decimal points displayed on axis
library(gsubfn)
library(ncdf4)

temp_range_annual = read.csv("temp range annual - 19 Mar 2023_with_spatial_blocks.csv")
temp_range_short_term = read.csv("temp range - 19 Mar 2023_with_spatial_blocks.csv")
br_diff_annual = read.csv("br diff e063 annual - 19 Mar 2023_with_spatial_blocks.csv")
br_diff_short_term = read.csv("br diff e063 - 19 Mar 2023_with_spatial_blocks.csv")

# short elevation[lon,lat]   (Contiguous storage) 
elevation <- raster("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/Bayesian modeling for sensitivity tests/Removing time series in the open ocean/gebco_2023_sub_ice_topo/GEBCO_2023_sub_ice_topo.nc",  varname = "elevation")
plot(elevation)
# Note: Cannot convert the raster to df, as: Error: cannot allocate vector of size 27.8 Gb

bathy = nc_open("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/Bayesian modeling for sensitivity tests/Removing time series in the open ocean/gebco_2023_sub_ice_topo/GEBCO_2023_sub_ice_topo.nc")
bathy
elevation =ncvar_get(bathy, "elevation")
lon =ncvar_get(bathy, "lon")
lat =ncvar_get(bathy, "lat")

metadata = read.csv("G:/Other computers/My Laptop/Documents/HOTS Database/metadata.csv")

metadata = metadata[which(metadata$depth_in_m <= 12.5), ]
metadata = metadata[which(metadata$depth_in_m != -999.0), ]
metadata = metadata[which(metadata$climate_classification != "Polar"), ]
metadata = metadata[which(metadata$measurement_freq_in_mins <= 60), ]
metadata = metadata[which(metadata$duration_in_years > 0.5), ] # WARNING: 
# metadata in MySQL gives diff results cos of lower accuracy
metadata <- metadata %>% 
  mutate(climate_classification = ifelse(abs(metadata$lat_in_degrees) < 20, "Tropical", 
                                         ifelse(abs(metadata$lat_in_degrees) > 35, "Temperate", "Subtropical")))
metadata$climate_classification = as.factor(metadata$climate_classification)

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

# match logger coordinates to gebco data
# store results
metadata2$original_index_lat = NA
metadata2$original_index_lon = NA

for (i in 1:length(metadata2$id)) {
  # choose a subset for faster computation
  chosen_original_indices_lon = which(lon > (metadata2$long_in_degrees[i] - 0.1) &
                                        lon < (metadata2$long_in_degrees[i] + 0.1))
  chosen_original_indices_lat = which(lat > (metadata2$lat_in_degrees[i] - 0.1) &
                                        lat < (metadata2$lat_in_degrees[i] + 0.1))
  temp_df = data.frame(lat_sub = vector(),
                       lon_sub = vector(),
                       original_index_lat = vector(),
                       original_index_lon = vector())
  for (j in 1:length(chosen_original_indices_lon)) {
    for (k in 1:length(chosen_original_indices_lat)) {
      temp_df2 = data.frame(lat_sub = lat[chosen_original_indices_lat[k]],
                           lon_sub = lon[chosen_original_indices_lon[j]],
                           original_index_lat = chosen_original_indices_lat[k],
                           original_index_lon = chosen_original_indices_lon[j])
      temp_df = rbind(temp_df, temp_df2)
    }
  }
  temp_df$dist= NA
  for(l in 1:length(temp_df$lon)) {
    temp_df$dist[l] = distm(c(temp_df$lon[l],
                              temp_df$lat[l]),
                            c(metadata2$long_in_degrees[i], 
                              metadata2$lat_in_degrees[i]),
                            fun = distHaversine)
  }
  chosen_index = which.min(temp_df$dist)
  metadata2$original_index_lat[i] = temp_df$original_index_lat[chosen_index]
  metadata2$original_index_lon[i] = temp_df$original_index_lon[chosen_index]
}

metadata2$gebco_depth = NA
for (i in 1:length(metadata2$id)) {
  metadata2$gebco_depth[i] = elevation[metadata2$original_index_lon[i], metadata2$original_index_lat[i]]
}

# filter again and remove open ocean loggers
metadata3 = metadata2[which(metadata2$gebco_depth > -200),]

######################
### Show World Map ###
######################

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

### Recenter ####

center <- 200 # positive values only - US centered view is 260

# shift coordinates to recenter loggers
metadata3$long.recenter <-  ifelse(metadata3$long_in_degrees < center - 180 , 
                                   metadata3$long_in_degrees + 360, metadata3$long_in_degrees) 

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

MAP<-mp + geom_point(data = metadata3, 
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
  annotate('text',x=-82.58167+360, y = -0.871389 , label = 'a', size =7,fontface='bold')+
  annotate('text',x=55 , y = -12 , label = 'b', size =7,fontface='bold')+
  annotate('text',x=-48.5451+360, y = -27.9354 , label = 'c', size =7,fontface='bold')+
  annotate('text',x=115.4616 , y = -20.9019 , label = 'd', size =7,fontface='bold')+
  annotate('text',x=148.0925 , y = -42.1239 , label = 'e', size =7,fontface='bold')+
  annotate('text',x=-72.30889+360, y = -41.49556 , label = 'f', size =7,fontface='bold')


MAP

# map looks good. Loggers that look in the middle of the oceans are from islands (e.g. Hawaii and Kiritimati)

#### end

# note that we have to recompute because the plot of the map removed loggers at the same coordinate but different depths
# match logger coordinates to gebco data
# store results
metadata$original_index_lat = NA
metadata$original_index_lon = NA
metadata$matched_lat = NA
metadata$matched_lon = NA

for (i in 1:length(metadata$id)) {
  # choose a subset for faster computation
  chosen_original_indices_lon = which(lon > (metadata$long_in_degrees[i] - 0.1) &
                                        lon < (metadata$long_in_degrees[i] + 0.1))
  chosen_original_indices_lat = which(lat > (metadata$lat_in_degrees[i] - 0.1) &
                                        lat < (metadata$lat_in_degrees[i] + 0.1))
  temp_df = data.frame(lat_sub = vector(),
                       lon_sub = vector(),
                       original_index_lat = vector(),
                       original_index_lon = vector())
  for (j in 1:length(chosen_original_indices_lon)) {
    for (k in 1:length(chosen_original_indices_lat)) {
      temp_df2 = data.frame(lat_sub = lat[chosen_original_indices_lat[k]],
                            lon_sub = lon[chosen_original_indices_lon[j]],
                            original_index_lat = chosen_original_indices_lat[k],
                            original_index_lon = chosen_original_indices_lon[j])
      temp_df = rbind(temp_df, temp_df2)
    }
  }
  temp_df$dist= NA
  for(l in 1:length(temp_df$lon)) {
    temp_df$dist[l] = distm(c(temp_df$lon[l],
                              temp_df$lat[l]),
                            c(metadata$long_in_degrees[i], 
                              metadata$lat_in_degrees[i]),
                            fun = distHaversine)
  }
  chosen_index = which.min(temp_df$dist)
  metadata$original_index_lat[i] = temp_df$original_index_lat[chosen_index]
  metadata$original_index_lon[i] = temp_df$original_index_lon[chosen_index]
  metadata$matched_lat[i] = temp_df$lat_sub[chosen_index]
  metadata$matched_lon[i] = temp_df$lon_sub[chosen_index]
}

metadata$gebco_depth = NA
for (i in 1:length(metadata$id)) {
  metadata$gebco_depth[i] = elevation[metadata$original_index_lon[i], metadata$original_index_lat[i]]
}

# save results
saveRDS(metadata, "G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/Bayesian modeling for sensitivity tests/Removing time series in the open ocean/metadata_with_gebco.Rds")
metadata = readRDS("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/Bayesian modeling for sensitivity tests/Removing time series in the open ocean/metadata_with_gebco.Rds")

# now filter the analysed data
temp_range_annual = merge(x = temp_range_annual, y = metadata[ , c("plot_id", "gebco_depth")], by = "plot_id", all.x=TRUE)
temp_range_annual = temp_range_annual[which(temp_range_annual$gebco_depth > -200),]
temp_range_short_term = merge(x = temp_range_short_term, y = metadata[ , c("plot_id", "gebco_depth")], by = "plot_id", all.x=TRUE)
temp_range_short_term = temp_range_short_term[which(temp_range_short_term$gebco_depth > -200),]
br_diff_annual = merge(x = br_diff_annual, y = metadata[ , c("plot_id", "gebco_depth")], by = "plot_id", all.x=TRUE)
br_diff_annual = br_diff_annual[which(br_diff_annual$gebco_depth > -200),]
br_diff_short_term = merge(x = br_diff_short_term, y = metadata[ , c("plot_id", "gebco_depth")], by = "plot_id", all.x=TRUE)
br_diff_short_term = br_diff_short_term[which(br_diff_short_term$gebco_depth > -200),]

remove_outliers <- function(x, na.rm=TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE)
  H <- 1.5 * IQR(x, na.rm=TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

asterisk_size = 3

###
# plot 1

temp_range1 = data.frame(mediann = vector(), climate_classification = vector(),
                         window = vector())
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$q_diurnal_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "QD"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$s_diurnal_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "SD"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$daily_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "D"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$weekly_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "W"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$biweekly_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "B"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_short_term$monthly_median, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "M"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range_annual$annual_median, 
                                            climate_classification = temp_range_annual$climate_classification,
                                            window = "Annual"))
temp_range1$window = as.factor(temp_range1$window)
temp_range1$window <- factor(temp_range1$window , 
                             levels=c("QD", "SD", "D", "W", "B","M","Annual"))
temp_range1$climate_classification <- factor(temp_range1$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

df_no_outliers <- temp_range1 %>% 
  group_by(climate_classification, window) %>%
  mutate(mediann = remove_outliers(mediann))

### PLOT 1
plot1 = ggplot(data=subset(temp_range1,window!='Annual'), aes(x=window, y=mediann, fill=climate_classification)) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("Median ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + theme(plot.title = element_text(hjust = 0.5,
                                                                                       size=15),
                                                             axis.title.x = element_blank(),
                                                             axis.ticks.x = element_blank(),
                                                             axis.text.x = element_blank(),
                                                             axis.text.y=element_text(size=12),
                                                             axis.title.y=element_text(size=14),
                                                             panel.border = element_rect(colour = "black",
                                                                                         fill=NA, size=1),
                                                             text=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             legend.position = c(0.28, 0.82), legend.spacing.y = unit(0.08, "mm"), legend.title=element_blank(),
                                                             legend.text=element_text(size=8),legend.key.size = unit(0.45, "cm"),
                                                             plot.margin=unit(c(0.2,0.2,0.3,0.2), "cm")) +
  ggtitle("Temperature range") +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))+scale_y_continuous(labels = label_number(accuracy = 1))  +
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "QD") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "QD") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "SD") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "SD") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "D") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=3, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "D") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "W") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "B") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "M") %>% .$mediann, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)

plot1 = plot1 + coord_cartesian(ylim = c(0,15))
plot1 

# Separate plot for annual
plot1a = ggplot(data=subset(temp_range1,window=='Annual'), aes(x=window, y=mediann, fill=climate_classification)) + 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("Median ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + theme(plot.title = element_text(hjust = 0.5,
                                                                                       size=15),
                                                             axis.title.x = element_blank(),
                                                             axis.ticks.x = element_blank(),
                                                             axis.text.x = element_blank(),
                                                             axis.text.y=element_text(size=12),
                                                             axis.title.y=element_blank(),
                                                             panel.border = element_rect(colour = "black",
                                                                                         fill=NA, size=1),
                                                             text=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             panel.background = element_rect(fill = "lightgrey",
                                                                                             colour = "lightgrey",
                                                                                             size = 0.5, linetype = "solid"),
                                                             legend.position = 'none',
                                                             plot.margin=unit(c(0.2,0.2,0.3,0.2), "cm")) +
  ggtitle("") +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.1,  y=0.92,
                                      gp=gpar(fontsize=15))))+
  scale_y_continuous(labels= label_number(accuracy = 1),breaks=c(0,5,10,15,20,25))+
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "Annual") %>% .$mediann, na.rm = TRUE) + 0.9, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "Annual") %>% .$mediann, na.rm = TRUE) + 0.9, 
           label="*",color="black",size=asterisk_size)

plot1a = plot1a + coord_cartesian(ylim = c(0,26))
plot1a


# plot 2

temp_range2 = data.frame(mediann = vector(), climate_classification = vector(),
                         window = vector())
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$q_diurnal_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "QD"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$s_diurnal_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "SD"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$daily_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "D"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$weekly_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "W"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$biweekly_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "B"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_short_term$monthly_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "M"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff_annual$annual_median, 
                                            climate_classification = br_diff_annual$climate_classification,
                                            window = "Annual"))
temp_range2$window = as.factor(temp_range2$window)
temp_range2$window <- factor(temp_range2$window , 
                             levels=c("QD", "SD", "D", "W", "B","M","Annual"))
temp_range2$climate_classification <- factor(temp_range2$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

df_no_outliers <- temp_range2 %>% 
  group_by(climate_classification, window) %>%
  mutate(mediann = remove_outliers(mediann))

### PLOT 2
plot2 = ggplot(data=subset(temp_range2,window!='Annual'), aes(x=window, y=mediann, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA)+ 
  labs(y = expression("Median (Wg"^-1*")"),fill = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=15),axis.title.x = element_blank(),
        axis.title.y = element_text(size=14),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill=NA, size=1),
        text=element_text(size=12),
        legend.position = "None",
        plot.margin=unit(c(0.2,0.7,0.3,0.2), "cm")) +
  ggtitle(paste("Difference in biological rates")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels = label_number(accuracy = 0.0000001)) + 
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "QD") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "QD") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "SD") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "SD") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "D") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=3, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "D") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=3.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "W") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "W") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "B") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "B") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "M") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=6, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "M") %>% .$mediann, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size) 

plot2 = plot2 + coord_cartesian(ylim = c(0,0.0000006))
plot2

# plot 2b (annual)
# Separate plot for annual
plot2a = ggplot(data=subset(temp_range2,window=='Annual'), aes(x=window, y=mediann, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+ 
  labs(y = "Median",
       fill = "Climate Classification") + theme_bw() + theme(plot.title = element_text(hjust = 0.5,
                                                                                       size=15),
                                                             axis.title.x = element_blank(),
                                                             axis.title.y = element_blank(),
                                                             axis.ticks.x = element_blank(),
                                                             axis.text.x = element_blank(),
                                                             axis.text.y=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             panel.border = element_rect(colour = "black",
                                                                                         fill=NA, size=1),
                                                             text=element_text(size=12),
                                                             panel.background = element_rect(fill = "lightgrey",
                                                                                             colour = "lightgrey",
                                                                                             size = 0.5, linetype = "solid"),
                                                             legend.position = "None",
                                                             plot.margin=unit(c(0.2,0.7,0.3,0.2), "cm")) +
  ggtitle(paste("")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.1,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels = label_number(accuracy = 0.0000001)) +
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "Annual") %>% .$mediann, na.rm = TRUE) + 0.00000003, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "Annual") %>% .$mediann, na.rm = TRUE) + 0.00000003, 
           label="*",color="black",size=asterisk_size) 

plot2a = plot2a + coord_cartesian(ylim = c(0,0.00000125))
plot2a

# plot 3

temp_range3 = data.frame(ninetieth_perc = vector(), climate_classification = vector(),
                         window = vector())
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$q_diurnal_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "QD"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$s_diurnal_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "SD"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$daily_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "D"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$weekly_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "W"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$biweekly_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "B"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_short_term$monthly_ninetieth_perc, 
                                            climate_classification = temp_range_short_term$climate_classification,
                                            window = "M"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range_annual$annual_ninetieth_perc, 
                                            climate_classification = temp_range_annual$climate_classification,
                                            window = "Annual"))
temp_range3$window = as.factor(temp_range3$window)
temp_range3$window <- factor(temp_range3$window , 
                             levels=c("QD", "SD", "D", "W", "B","M","Annual"))
temp_range3$climate_classification <- factor(temp_range3$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

df_no_outliers <- temp_range3 %>% 
  group_by(climate_classification, window) %>%
  mutate(ninetieth_perc = remove_outliers(ninetieth_perc))

plot3 = ggplot(data=subset(temp_range3,window!='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA)+ 
  labs(x = "Temporal Window", y = expression("90"^th*" percentile ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + theme(panel.border = element_rect(colour = "black", 
                                                                                         fill=NA, size=1),
                                                             text=element_text(size=12),
                                                             axis.text=element_text(size=12),
                                                             axis.title.y=element_text(size=14),
                                                             axis.title.x=element_blank(),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             legend.position = "None",
                                                             plot.margin=unit(c(0.1,0.2,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels= label_number(accuracy = 1),breaks=c(0,5,10,15,20,25)) +
  annotate(geom="text", x=3.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "W") %>% .$ninetieth_perc, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "B") %>% .$ninetieth_perc, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "M") %>% .$ninetieth_perc, na.rm = TRUE) + 0.3, 
           label="*",color="black",size=asterisk_size)

plot3 = plot3 + coord_cartesian(ylim = c(0,15))
plot3

# Plot 3a annual only
plot3a = ggplot(data=subset(temp_range3,window=='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+ 
  labs(x = "Temporal Window", y = expression("90"^th*" percentile ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + theme(panel.border = element_rect(colour = "black", 
                                                                                         fill=NA, size=1),
                                                             text=element_text(size=12),
                                                             axis.title.y=element_blank(),
                                                             axis.title.x=element_blank(),
                                                             axis.text=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             legend.position = "None",
                                                             panel.background = element_rect(fill = "lightgrey",
                                                                                             colour = "lightgrey",
                                                                                             size = 0.5, linetype = "solid"),
                                                             plot.margin=unit(c(0.1,0.2,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.1,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels= label_number(accuracy = 1),breaks=c(0,5,10,15,20,25))+
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "Annual") %>% .$ninetieth_perc, na.rm = TRUE) + 0.9, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "Annual") %>% .$ninetieth_perc, na.rm = TRUE) + 0.9, 
           label="*",color="black",size=asterisk_size)

plot3a = plot3a + coord_cartesian(ylim = c(0,26))
plot3a

# plot 4

temp_range4 = data.frame(ninetieth_perc = vector(), climate_classification = vector(),
                         window = vector())
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$q_diurnal_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "QD"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$s_diurnal_median, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "SD"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$daily_ninetieth_perc, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "D"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$weekly_ninetieth_perc, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "W"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$biweekly_ninetieth_perc, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "B"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_short_term$monthly_ninetieth_perc, 
                                            climate_classification = br_diff_short_term$climate_classification,
                                            window = "M"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff_annual$annual_ninetieth_perc, 
                                            climate_classification = br_diff_annual$climate_classification,
                                            window = "Annual"))
temp_range4$window = as.factor(temp_range4$window)
temp_range4$window <- factor(temp_range4$window , 
                             levels=c("QD", "SD", "D", "W", "B","M","Annual"))
temp_range4$climate_classification <- factor(temp_range4$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

df_no_outliers <- temp_range4 %>% 
  group_by(climate_classification, window) %>%
  mutate(ninetieth_perc = remove_outliers(ninetieth_perc))

plot4 = ggplot(data=subset(temp_range4,window!='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA)+ 
  labs(x = "Temporal Window", y = expression("90"^th*" percentile (Wg"^-1*")"),
       fill = "Climate Classification") + theme_bw() + theme(panel.border = element_rect(colour = "black", 
                                                                                         fill=NA, size=1),
                                                             axis.title = element_text(size=14),
                                                             axis.text=element_text(size=12),
                                                             axis.title.x=element_blank(),
                                                             text=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             legend.position = 'none',
                                                             plot.margin=unit(c(0.1,0.7,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))+
  scale_y_continuous(labels = label_number(accuracy = 0.0000001))+
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "QD") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "QD") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "SD") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "SD") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=2.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "D") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=3, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "D") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=3.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "W") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "W") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=4.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "B") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "B") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=5.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "M") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=6, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "M") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000001, 
           label="*",color="black",size=asterisk_size)

plot4 = plot4 + coord_cartesian(ylim = c(0,0.0000006))
plot4

# Plot 4a annual only
plot4a = ggplot(data=subset(temp_range4,window=='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+ 
  labs(x = "Temporal Window", y = "90th percentile",
       fill = "Climate Classification") + theme_bw() + theme(panel.border = element_rect(colour = "black", 
                                                                                         fill=NA, size=1),
                                                             axis.title = element_blank(),
                                                             axis.text=element_text(size=12),
                                                             text=element_text(size=12),
                                                             panel.grid.major = element_blank(), 
                                                             panel.grid.minor = element_blank(),
                                                             panel.background = element_rect(fill = "lightgrey",
                                                                                             colour = "lightgrey",
                                                                                             size = 0.5, linetype = "solid"),
                                                             legend.position = "None",
                                                             plot.margin=unit(c(0.1,0.7,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("h")), x=0.1,  y=0.92,
                                      gp=gpar(fontsize=15))))+
  scale_y_continuous(labels = label_number(accuracy = 0.0000001))+
  annotate(geom="text", x=0.75, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical",
                                           window == "Annual") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000003, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=1, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical",
                                           window == "Annual") %>% .$ninetieth_perc, na.rm = TRUE) + 0.00000003, 
           label="*",color="black",size=asterisk_size)

plot4a = plot4a + coord_cartesian(ylim = c(0,0.00000125))
plot4a

grid.arrange(plot1, plot1a, plot2, plot2a, plot3, plot3a, plot4, plot4a, 
             ncol = 4, widths = c(0.8,0.25,0.7,0.25), heights = c(2.5, 2.5))

ggsave(filename="FigS5_with_asterisks.png",height=5, width=11, units="in", 
       plot=grid.arrange(plot1, plot1a, plot2, plot2a, plot3, plot3a, plot4, plot4a, ncol = 4, 
                         widths = c(4,1.5, 4.5,2), heights = c(2.5, 2.5)), device="png")

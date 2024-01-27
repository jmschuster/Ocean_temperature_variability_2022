rm(list = ls())

setwd("G:/Other computers/My Laptop/Documents/HOTS Database")

library(RMySQL)
library(dplyr)

metadata = read.csv("metadata.csv")

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

# statistics
metadata_grouped <- metadata %>% 
  mutate(plot_id = gsub( " .*$", "", metadata$plot_id))
metadata_grouped = metadata_grouped[match(unique(metadata_grouped$plot_id), metadata_grouped$plot_id),]
table(metadata_grouped$climate_classification)
length(unique(metadata[,c('long_in_degrees','lat_in_degrees')])$long_in_degrees)
length(unique(metadata_grouped[,c('long_in_degrees','lat_in_degrees')])$long_in_degrees)
max(metadata$duration_in_years)
max(metadata$depth_in_m)
max(metadata$lat_in_degrees)
min(metadata$lat_in_degrees)
min(metadata$start_date)
max(metadata$end_date)
min(metadata$duration_in_days)
max(metadata$measurement_freq_in_mins)
min(metadata$measurement_freq_in_mins)

sum(metadata$no_of_data)
net = (1-metadata$temp_degC_percentage_missing_values/100)*metadata$no_of_data
sum(net)

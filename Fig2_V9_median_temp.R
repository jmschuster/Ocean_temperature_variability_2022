rm(list = ls())

#setwd("C:/Users/Owner/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")
setwd("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")
#setwd("C:/Users/dlcyli/OneDrive/Ocean_temperature_variability_2022")

library(gamm4)
library(ggplot2)
library(dplyr)
# library(visreg)
library(gridExtra)
library(grid)
library(ggpubr)
library(voxel)
library(scales) # to specify number of decimal points displayed on axis
library(gsubfn)

temp_range_annual = read.csv("temp range annual - 19 Mar 2023_with_spatial_blocks.csv")
temp_range_short_term = read.csv("temp range - 19 Mar 2023_with_spatial_blocks.csv")
br_diff_annual = read.csv("br diff e063 annual - 19 Mar 2023_with_spatial_blocks.csv")
br_diff_short_term = read.csv("br diff e063 - 19 Mar 2023_with_spatial_blocks.csv")

temp_range_annual$pseudo_climate = temp_range_annual$climate_classification
temp_range_annual$pseudo_climate[which(temp_range_annual$pseudo_climate == "Tropical")] = 10
temp_range_annual$pseudo_climate[which(temp_range_annual$pseudo_climate == "Subtropical")] = 27.5
temp_range_annual$pseudo_climate[which(temp_range_annual$pseudo_climate == "Temperate")] = 45
temp_range_annual$pseudo_climate = as.double(temp_range_annual$pseudo_climate)

temp_range_short_term$pseudo_climate = temp_range_short_term$climate_classification
temp_range_short_term$pseudo_climate[which(temp_range_short_term$pseudo_climate == "Tropical")] = 10
temp_range_short_term$pseudo_climate[which(temp_range_short_term$pseudo_climate == "Subtropical")] = 27.5
temp_range_short_term$pseudo_climate[which(temp_range_short_term$pseudo_climate == "Temperate")] = 45
temp_range_short_term$pseudo_climate = as.double(temp_range_short_term$pseudo_climate)

# statistics
metadata_grouped2 <- temp_range_annual %>% 
  mutate(plot_id = gsub( " .*$", "", temp_range_annual$plot_id))
metadata_grouped2 = metadata_grouped2[match(unique(metadata_grouped2$plot_id), metadata_grouped2$plot_id),]
table(metadata_grouped2$climate_classification)

# #N.B: Open world map JMS.R in G:\Other computers\My Laptop\Documents\HOTS Database\Final Paper\RMarkDown\Sensitivity Test
# # to get metadata_grouped for the statistics of the studies in tropical regions out of the 169
# metadata_grouped3 = metadata_grouped[which(metadata_grouped$plot_id %in% metadata_grouped2$plot_id),]
# table(metadata_grouped3$climate_classification)
# tropics_only = metadata_grouped3[which(metadata_grouped3$climate_classification == "Tropical"),]
# table(tropics_only$data_source)
# tropics_only$data_source_complete = NA
# for (i in 1:length(tropics_only$id)) {
#   if (tropics_only$data_source[i] == "Same as PI") {
#     tropics_only$data_source_complete[i] = tropics_only$PI_contributor[i]
#   } else {
#     tropics_only$data_source_complete[i] = tropics_only$data_source[i]
#   }
# }
# table(tropics_only$data_source_complete)

remove_outliers <- function(x, na.rm=TRUE) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm=TRUE)
  H <- 1.5 * IQR(x, na.rm=TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

scientific_10 <- function(x) {
  ifelse(x==0, "0", parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x))))
}

alpha_value = 0.1
title_size = 10
axis_title_size = 9
axis_text_size = 7
panel_label_annual_spacing = 0.2
asterisk_size = 5

###
# q_diurnal_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(q_diurnal_median = remove_outliers(q_diurnal_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))

gamm_daily_median <- gamm4(q_diurnal_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data=df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot1 = GAMMplot + geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
                                aes(x=pseudo_climate, y=q_diurnal_median, color=climate_classification)) + 
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_median)),], 
                             aes(x=lat_in_degrees, y=q_diurnal_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range (" ^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.title = element_text(size=axis_title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0,0.3,0.2), "cm")) +
  ggtitle("Quarter-diurnal (6 hrs)") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$q_diurnal_median, na.rm = TRUE) + 
             max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$q_diurnal_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) 

plot1 = plot1 + coord_cartesian(ylim = c(0,1.25))
plot1 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_median)),]$q_diurnal_median) 

# s_diurnal_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(s_diurnal_median = remove_outliers(s_diurnal_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(s_diurnal_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot2 = GAMMplot + 
  geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=s_diurnal_median, color=climate_classification)) + 
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_median)),], 
                             aes(x=lat_in_degrees, y=s_diurnal_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range ("^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.2,0.3,0.5), "cm")) +
  ggtitle("Semi-diurnal (12 hrs)") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$s_diurnal_median, na.rm = TRUE) + 
             max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$s_diurnal_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)

plot2 = plot2 + coord_cartesian(ylim = c(0,2))
plot2 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_median)),]$s_diurnal_median) 

# daily_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(daily_median = remove_outliers(daily_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(daily_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$daily_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot3 = GAMMplot + 
  geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=daily_median, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$daily_median)),], 
                              aes(x=lat_in_degrees, y=daily_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range ("^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.4,0.3,0.3), "cm")) +
  ggtitle("Diurnal (24 hrs)") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$daily_median, na.rm = TRUE) + 
             max(df_no_outliers %>% 
                   filter(climate_classification == "Subtropical") %>% 
                   .$daily_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = label_number(accuracy = 0.1))

plot3 = plot3 + coord_cartesian(ylim = c(0,3))
plot3 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$daily_median)),]$daily_median) 

# legend
plot4 = GAMMplot + geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$daily_median)),], 
                              aes(x=lat_in_degrees, y=daily_median, color=climate_classification)) +
  labs(color = "Climate Classification") + 
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) 

legend= get_legend(plot4)
plot4 = as_ggplot(legend)

# weekly_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(weekly_median = remove_outliers(weekly_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(weekly_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$weekly_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot5 = GAMMplot + 
  geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=weekly_median, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$weekly_median)),], 
                              aes(x=lat_in_degrees, y=weekly_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range ("^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.6,0.3,0.1), "cm")) +
  ggtitle("Weekly") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$weekly_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$weekly_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$weekly_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$weekly_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = label_number(accuracy = 0.1))

plot5 = plot5 + coord_cartesian(ylim = c(0,6))
plot5 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$weekly_median)),]$weekly_median) 

# biweekly_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(biweekly_median = remove_outliers(biweekly_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(biweekly_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$biweekly_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot6 = GAMMplot + 
  geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=biweekly_median, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$biweekly_median)),], 
                              aes(x=lat_in_degrees, y=biweekly_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range (" ^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0,0,0.1,0.2), "cm")) +
  ggtitle("Bi-weekly") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$biweekly_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$biweekly_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$biweekly_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$biweekly_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = label_number(accuracy = 0.1))

plot6 = plot6 + coord_cartesian(ylim = c(0,7.5))
plot6 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$biweekly_median)),]$biweekly_median) 

# monthly_median
df_no_outliers <- temp_range_short_term %>% 
  group_by(climate_classification) %>%
  mutate(monthly_median = remove_outliers(monthly_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(monthly_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$monthly_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot7 = GAMMplot + 
  geom_boxplot(data = temp_range_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=monthly_median, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$monthly_median)),], 
                              aes(x=lat_in_degrees, y=monthly_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range ("^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0,0.2,0.1,0.4), "cm")) +
  ggtitle("Monthly") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$monthly_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$monthly_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)

plot7 = plot7 + coord_cartesian(ylim = c(0,10))
plot7 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$monthly_median)),]$monthly_median) 

# annual_median
df_no_outliers <- temp_range_annual %>% 
  group_by(climate_classification) %>%
  mutate(annual_median = remove_outliers(annual_median))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_median <- gamm4(annual_median ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$annual_median)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_median, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot8 = GAMMplot + 
  geom_boxplot(data = temp_range_annual, outlier.shape=NA, 
               aes(x=pseudo_climate, y=annual_median, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$annual_median)),], 
                              aes(x=lat_in_degrees, y=annual_median, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("Median of temperature range ("^o*"C)"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -1, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0,0.4,0.1,0.2), "cm")) +
  ggtitle("Annual") +
  scale_color_manual(values=c("#D5A458", "#79CEDC", "#F65058FF")) + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$annual_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$annual_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$annual_median, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$annual_median, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = label_number(accuracy = 0.1))


plot8 = plot8 + coord_cartesian(ylim = c(0,21))
plot8 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$annual_median)),]$annual_median) 

ggsave(filename="Fig2.png",height=5, width=11, units="in", 
       plot=grid.arrange(plot1, plot2, plot3, plot5, plot6, plot7, plot8, plot4, ncol = 4), 
       device="png")

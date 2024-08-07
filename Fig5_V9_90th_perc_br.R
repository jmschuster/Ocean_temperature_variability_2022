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
library(corto)

br_diff_annual = read.csv("br diff e063 annual - 19 Mar 2023_with_spatial_blocks.csv")
br_diff_short_term = read.csv("br diff e063 - 19 Mar 2023_with_spatial_blocks.csv")

br_diff_annual$pseudo_climate = br_diff_annual$climate_classification
br_diff_annual$pseudo_climate[which(br_diff_annual$pseudo_climate == "Tropical")] = 10
br_diff_annual$pseudo_climate[which(br_diff_annual$pseudo_climate == "Subtropical")] = 27.5
br_diff_annual$pseudo_climate[which(br_diff_annual$pseudo_climate == "Temperate")] = 45
br_diff_annual$pseudo_climate = as.double(br_diff_annual$pseudo_climate)

br_diff_short_term$pseudo_climate = br_diff_short_term$climate_classification
br_diff_short_term$pseudo_climate[which(br_diff_short_term$pseudo_climate == "Tropical")] = 10
br_diff_short_term$pseudo_climate[which(br_diff_short_term$pseudo_climate == "Subtropical")] = 27.5
br_diff_short_term$pseudo_climate[which(br_diff_short_term$pseudo_climate == "Temperate")] = 45
br_diff_short_term$pseudo_climate = as.double(br_diff_short_term$pseudo_climate)

# statistics
metadata_grouped <- br_diff_annual %>% 
  mutate(plot_id = gsub( " .*$", "", br_diff_annual$plot_id))
metadata_grouped = metadata_grouped[match(unique(metadata_grouped$plot_id), metadata_grouped$plot_id),]
table(metadata_grouped$climate_classification)

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

scientific_10_1dp <- function(v, decimals = 1) {
  v<-signif(v)
  vv<-format(v,scientific=TRUE)
  v1<-gsub("e.+","",vv)
  v2<-gsub(".+e","",vv)
  v2<-gsub("-0+","-",v2)
  v2<-gsub("\\+0","+",v2)
  v2<-gsub("\\++","",v2)
  
  vexpr<-vector("expression",length(v))
  for(i in 1:length(vv)){
    bq<-as.expression(bquote(.(sprintf("%.1f", as.integer(v1[i])))~x~10^.(v2[i])))
    vexpr[i]<-bq
  }
  return(vexpr)
}

alpha_value = 0.1
title_size = 10
axis_title_size = 9
axis_text_size = 7
panel_label_annual_spacing = 0.2
asterisk_size = 5

###
# q_diurnal_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(q_diurnal_ninetieth_perc = remove_outliers(q_diurnal_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))

gamm_daily_ninetieth_perc <- gamm4(q_diurnal_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data=df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot1 = GAMMplot + geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
                                aes(x=pseudo_climate, y=q_diurnal_ninetieth_perc, color=climate_classification)) + 
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_ninetieth_perc)),], 
                             aes(x=lat_in_degrees, y=q_diurnal_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = "Absolute latitude", y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.title = element_text(size=axis_title_size),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0,0.3,0.2), "cm")) +
  ggtitle("Quarter-diurnal (6 hrs)") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$q_diurnal_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$q_diurnal_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$q_diurnal_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$q_diurnal_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scinot) 

plot1 = plot1 + coord_cartesian(ylim = c(0,0.00000015))
plot1 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$q_diurnal_ninetieth_perc)),]$q_diurnal_ninetieth_perc)

# s_diurnal_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(s_diurnal_ninetieth_perc = remove_outliers(s_diurnal_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(s_diurnal_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot2 = GAMMplot + 
  geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=s_diurnal_ninetieth_perc, color=climate_classification)) + 
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_ninetieth_perc)),], 
                             aes(x=lat_in_degrees, y=s_diurnal_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = "Absolute latitude", y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.2,0.3,0.5), "cm")) +
  ggtitle("Semi-diurnal (12 hrs)") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$s_diurnal_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$s_diurnal_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$s_diurnal_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$s_diurnal_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scinot)

plot2 = plot2 + coord_cartesian(ylim = c(0,0.0000002))
plot2 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$s_diurnal_ninetieth_perc)),]$s_diurnal_ninetieth_perc)

# daily_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(daily_ninetieth_perc = remove_outliers(daily_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(daily_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$daily_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot3 = GAMMplot + 
  geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=daily_ninetieth_perc, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$daily_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=daily_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = "Absolute latitude", y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.4,0.3,0.3), "cm")) +
  ggtitle("Diurnal (24 hrs)") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$daily_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$daily_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$daily_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$daily_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scinot)

plot3 = plot3 + coord_cartesian(ylim = c(0,0.00000025))
plot3 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$daily_ninetieth_perc)),]$daily_ninetieth_perc)

# legend
plot4 = ggplot() + geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$daily_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=daily_ninetieth_perc, color=climate_classification)) +
  labs(color = "Climate Classification") + 
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) 

legend= get_legend(plot4)
plot4 = as_ggplot(legend)

# weekly_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(weekly_ninetieth_perc = remove_outliers(weekly_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(weekly_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$weekly_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot5 = GAMMplot + 
  geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=weekly_ninetieth_perc, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$weekly_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=weekly_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0.3,0.6,0.3,0.1), "cm")) +
  ggtitle("Weekly") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$weekly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$weekly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$weekly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$weekly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scientific_10_1dp)

plot5 = plot5 + coord_cartesian(ylim = c(0,0.0000004))
plot5 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$weekly_ninetieth_perc)),]$weekly_ninetieth_perc)

# biweekly_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(biweekly_ninetieth_perc = remove_outliers(biweekly_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(biweekly_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$biweekly_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot6 = GAMMplot + 
  geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=biweekly_ninetieth_perc, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$biweekly_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=biweekly_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
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
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$biweekly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$biweekly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$biweekly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$biweekly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scientific_10_1dp)

plot6 = plot6 + coord_cartesian(ylim = c(0,0.00000045))
plot6 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$biweekly_ninetieth_perc)),]$biweekly_ninetieth_perc)

# monthly_ninetieth_perc
df_no_outliers <- br_diff_short_term %>% 
  group_by(climate_classification) %>%
  mutate(monthly_ninetieth_perc = remove_outliers(monthly_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(monthly_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks/plot_id), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$monthly_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot7 = GAMMplot + 
  geom_boxplot(data = br_diff_short_term, outlier.shape=NA, 
               aes(x=pseudo_climate, y=monthly_ninetieth_perc, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$monthly_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=monthly_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0,0.2,0.1,0.5), "cm")) +
  ggtitle("Monthly") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$monthly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$monthly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$monthly_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$monthly_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scientific_10_1dp)


plot7 = plot7 + coord_cartesian(ylim = c(0,0.00000055))
plot7 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$monthly_ninetieth_perc)),]$monthly_ninetieth_perc)

# annual_ninetieth_perc
df_no_outliers <- br_diff_annual %>% 
  group_by(climate_classification) %>%
  mutate(annual_ninetieth_perc = remove_outliers(annual_ninetieth_perc))

df_no_outliers$lat_in_degrees = abs(df_no_outliers$lat_in_degrees)
df_no_outliers$climate_classification <- factor(df_no_outliers$climate_classification , 
                                                levels=c("Tropical", "Subtropical", "Temperate"))
gamm_daily_ninetieth_perc <- gamm4(annual_ninetieth_perc ~ s(lat_in_degrees) + s(depth_in_m),
                           random = ~ (1|spatial_blocks), 
                           data = df_no_outliers[which(!is.na(df_no_outliers$annual_ninetieth_perc)),])

GAMMplot <- plotGAMM(gammFit = gamm_daily_ninetieth_perc, smooth.cov = "lat_in_degrees", groupCovs = NULL, plotCI = T)

plot8 = GAMMplot + 
  geom_boxplot(data = br_diff_annual, outlier.shape=NA, 
               aes(x=pseudo_climate, y=annual_ninetieth_perc, color=climate_classification)) +
  geom_point(data=df_no_outliers[which(!is.na(df_no_outliers$annual_ninetieth_perc)),], 
                              aes(x=lat_in_degrees, y=annual_ninetieth_perc, color=climate_classification), alpha=alpha_value) +
  labs(x = expression("Absolute latitude (" ^o* ")"), y = expression("90"^th*" percentile of biological rates (Wg"^-1*")"),
       color = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=title_size),
        axis.text.x=element_text(size=axis_text_size),
        axis.text.y=element_text(size=axis_text_size),
        axis.title.y=element_blank(),
        axis.title = element_text(size=axis_title_size),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none", 
        plot.margin=unit(c(0,0.4,0.1,0.3), "cm")) +
  ggtitle("Annual") +
  scale_color_manual(values=c("#F65058FF", "#D5A458", "#79CEDC")) + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  annotate(geom="text", x=10, 
           y=max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                   .$annual_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$annual_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size)+
  annotate(geom="text", x=27.5, 
           y=max(df_no_outliers %>% filter(climate_classification == "Subtropical") %>% 
                   .$annual_ninetieth_perc, na.rm = TRUE) + 
             max(df_no_outliers %>% filter(climate_classification == "Tropical") %>% 
                     .$annual_ninetieth_perc, na.rm = TRUE)/10, 
           label="*",color="black",size=asterisk_size) +
  scale_y_continuous(labels = scientific_10_1dp)


plot8 = plot8 + coord_cartesian(ylim = c(0,0.0000011))
plot8 

#sample size
length(df_no_outliers[which(!is.na(df_no_outliers$annual_ninetieth_perc)),]$annual_ninetieth_perc)

ggsave(filename="Fig5.pdf",height=5, width=11, units="in", 
       plot=grid.arrange(plot1, plot2, plot3, plot5, plot6, plot7, plot8, plot4, ncol = 4))
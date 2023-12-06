rm(list = ls())

#setwd("C:/Users/Owner/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")
setwd("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test")

library(ggplot2)
library(dplyr)
# library(visreg)
library(gridExtra)
library(grid)
# library(showtext)
# library(emojifont)
library(scales) # to specify number of decimal points displayed on axis

temp_range_annual = read.csv("temp range annual Conservative more than 3 years - 19 Mar 23.csv")
temp_range_short_term = read.csv("temp range conservative more than 3 years - 19 Mar 23.csv")
br_diff_annual = read.csv("br diff e063 annual conservative more than 3 years - 19 Mar 23.csv")
br_diff_short_term = read.csv("br diff e063 conservative more than 3 years - 19 Mar 23.csv")

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
                                      gp=gpar(fontsize=15))))+scale_y_continuous(labels = label_number(accuracy = 1))

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
                                      gp=gpar(fontsize=15))))+scale_y_continuous(labels = label_number(accuracy = 1)) 

plot1a = plot1a + coord_cartesian(ylim = c(0,25.00))
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
  scale_y_continuous(labels = label_number(accuracy = 0.0000001)) 

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
  scale_y_continuous(labels = label_number(accuracy = 0.0000001)) 

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
  scale_y_continuous(labels= label_number(accuracy = 1),breaks=c(0,5,10,15,20,25))

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
  scale_y_continuous(labels= label_number(accuracy = 1),breaks=c(0,5,10,15,20,25))

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
  scale_y_continuous(labels = label_number(accuracy = 0.0000001))

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
  scale_y_continuous(labels = label_number(accuracy = 0.0000001))

plot4a = plot4a + coord_cartesian(ylim = c(0,0.00000125))
plot4a

grid.arrange(plot1, plot1a, plot2, plot2a, plot3, plot3a, plot4, plot4a, 
             ncol = 4, widths = c(0.8,0.25,0.7,0.25), heights = c(2.5, 2.5))

ggsave(filename="FigS3_V4.png",height=5, width=11, units="in", 
       plot=grid.arrange(plot1, plot1a, plot2, plot2a, plot3, plot3a, plot4, plot4a, ncol = 4, 
                         widths = c(4,1.5, 4.5,2), heights = c(2.5, 2.5)), device="png")

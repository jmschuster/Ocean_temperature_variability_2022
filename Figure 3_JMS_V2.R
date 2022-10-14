rm(list = ls())

setwd("~/Documents/GitHub/Ocean_temperature_variability_2022/")


library(ggplot2)
library(dplyr)
library(visreg)
library(gridExtra)
library(grid)
library(showtext)
library(emojifont)
library(scales) # to specify number of decimal points displayed on axis

temp_range = read.csv("temp range_V2_conservative.csv") # using new file version that includes seasonal/annual var
br_diff = read.csv("br_diff_V2_conservative.csv")
bayesian_coeffs = read.csv('Bayesian_coeffs.csv') # coefficients for temp range
bayesian_coeffs_br_diff = read.csv('Bayesian_coeffs - br_diff.csv')

colnames(bayesian_coeffs_br_diff)[4] = "mediann" # match col name with temp_range df
colnames(bayesian_coeffs)[4] = "mediann" # match col name with temp_range df

scaleFUN <- function(x) sprintf("%.2f", x) # function to specify number of decimal points for axis labels

# 90th percentile coefficients
bayesian_coeffs90<-bayesian_coeffs %>% filter(X>15)
colnames(bayesian_coeffs90)[4] = "ninetieth_perc" # match col name with temp_range df

bayesian_coeffs_br_diff90<-bayesian_coeffs_br_diff %>% filter(X>15)
colnames(bayesian_coeffs_br_diff90)[4] = "ninetieth_perc" # match col name with temp_range df

bayesian_coeffs$climate_classification <- factor(bayesian_coeffs$climate_classification ,
                                                 levels=c("Tropical", "Subtropical", "Temperate"))

bayesian_coeffs_br_diff$climate_classification <- factor(bayesian_coeffs_br_diff$climate_classification ,
                                                         levels=c("Tropical", "Subtropical", "Temperate"))

### FILTER MISSING VALUES IN ANNUAL
temp_range = temp_range[which(!is.na(temp_range$annual_a_mean)),]
table(temp_range$climate_classification)
br_diff = br_diff[which(!is.na(br_diff$annual_a_mean)),]
table(br_diff$climate_classification)

###
# plot 1

temp_range1 = data.frame(mediann = vector(), climate_classification = vector(),
                         window = vector())
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range$daily_median, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Daily"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range$weekly_median, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Weekly"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range$biweekly_median, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Biweekly"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range$monthly_median, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Monthly"))
temp_range1 = rbind(temp_range1, data.frame(mediann = temp_range$annual_median, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Annual"))
temp_range1$window = as.factor(temp_range1$window)
temp_range1$window <- factor(temp_range1$window , 
                             levels=c("Daily", "Weekly", "Biweekly","Monthly","Annual"))
temp_range1$climate_classification <- factor(temp_range1$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

### PLOT 1
plot1 = ggplot(data=subset(temp_range1,window!='Annual'), aes(x=window, y=mediann, fill=climate_classification)) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("Median ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=15),axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14,margin = margin(t = 0, r = 10, b = 0, l = -8)),panel.border = element_rect(colour = "black", fill=NA, size=1),
        text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "None",
        plot.margin=unit(c(0.2,0.2,0.3,0.5), "cm")) +
  ggtitle("Temperature range") + 
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))+scale_y_continuous(labels=scaleFUN)+ 
  annotate(geom="text", x=1, y=3, label="*",color="black",size=5)+
  annotate(geom="text", x=2, y=5.5, label="*",color="black",size=5)+
  annotate(geom="text", x=3, y=7, label="*",color="black",size=5)+
  annotate(geom="text", x=3.75, y=5, label="*",color="black",size=5)


plot1 = plot1 + coord_cartesian(ylim = c(0,8.00))
plot1 

# Separate plot for annual
plot1a = ggplot(data=subset(temp_range1,window=='Annual'), aes(x=window, y=mediann, fill=climate_classification)) + 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("Median ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5,size=15), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        axis.text.y=element_text(size=12), axis.title.y=element_blank(),
        panel.border = element_rect(colour = "black",  fill=NA, size=1),
        text=element_text(size=12),panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey", 
                                        size = 0.5, linetype = "solid"),
        legend.position = 'none',plot.margin=unit(c(0.2,0.2,0.3,0.2), "cm")) +
  ggtitle("") + scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.14,  y=0.92,
                                      gp=gpar(fontsize=15))))+scale_y_continuous(labels=scaleFUN)+ 
  annotate(geom="text", x=1, y=17, label="*",color="black",size=5)+
  annotate(geom="text", x=0.75, y=15, label="*",color="black",size=5)

plot1a = plot1a + coord_cartesian(ylim = c(0,25.00))
plot1a


# plot 2

temp_range2 = data.frame(mediann = vector(), climate_classification = vector(),
                         window = vector())
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff$daily_median, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Daily"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff$weekly_median, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Weekly"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff$biweekly_median, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Biweekly"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff$monthly_median, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Monthly"))
temp_range2 = rbind(temp_range2, data.frame(mediann = br_diff$annual_median, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Annual"))
temp_range2$window = as.factor(temp_range2$window)
temp_range2$window <- factor(temp_range2$window , 
                             levels=c("Daily", "Weekly", "Biweekly","Monthly","Annual"))
temp_range2$climate_classification <- factor(temp_range2$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

### PLOT 2
plot2 = ggplot(data=subset(temp_range2,window!='Annual'), aes(x=window, y=mediann, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA)+
  labs(y = expression("Median ("^o*"C)"),fill = "Climate Classification") + theme_bw() + 
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
                                                             plot.margin=unit(c(0.2,0.2,0.3,0.2), "cm")) +
  ggtitle(paste("Range of biological rates")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels = label_number(accuracy = 0.0001))+ 
  annotate(geom="text", x=1, y=0.0002, label="*",color="black",size=5)+
  annotate(geom="text", x=0.75, y=0.0002, label="*",color="black",size=5)+
  annotate(geom="text", x=2, y=0.00035, label="*",color="black",size=5)+
  annotate(geom="text", x=1.75, y=0.0003, label="*",color="black",size=5)+
  annotate(geom="text", x=3, y=0.00041, label="*",color="black",size=5)+
  annotate(geom="text", x=2.75, y=0.00038, label="*",color="black",size=5)+
  annotate(geom="text", x=4, y=0.00057, label="*",color="black",size=5)+
  annotate(geom="text", x=3.75, y=0.00052, label="*",color="black",size=5)

plot2 = plot2 + coord_cartesian(ylim = c(0,0.0006))
plot2

# plot 2b (annual)
# Separate plot for annual
plot2a = ggplot(data=subset(temp_range2,window=='Annual'), aes(x=window, y=mediann, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA) + 
  labs(y = "Median",fill = "Climate Classification") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size=15), axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),axis.text.y=element_text(size=12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        text=element_text(size=12), panel.background = element_rect(fill = "lightgrey",colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),legend.position = "None",
        plot.margin=unit(c(0.2,0.7,0.3,0.1), "cm")) + ggtitle(paste("")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.12,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  scale_y_continuous(labels = label_number(accuracy = 0.0001))+
  annotate(geom="text", x=1, y=0.0014, label="*",color="black",size=5)
  

# (0.1,0.7,0.2,0.1) = plot (g) margins
plot2a = plot2a + coord_cartesian(ylim = c(0,0.0015))
plot2a

# plot 3

temp_range3 = data.frame(ninetieth_perc = vector(), climate_classification = vector(),
                         window = vector())
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range$daily_ninetieth_perc, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Daily"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range$weekly_ninetieth_perc, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Weekly"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range$biweekly_ninetieth_perc, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Biweekly"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range$monthly_ninetieth_perc, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Monthly"))
temp_range3 = rbind(temp_range3, data.frame(ninetieth_perc = temp_range$annual_ninetieth_perc, 
                                            climate_classification = temp_range$climate_classification,
                                            window = "Annual"))
temp_range3$window = as.factor(temp_range3$window)
temp_range3$window <- factor(temp_range3$window , 
                             levels=c("Daily", "Weekly", "Biweekly","Monthly","Annual"))
temp_range3$climate_classification <- factor(temp_range3$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

plot3 = ggplot(data=subset(temp_range3,window!='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA) +
  labs(x = "Temporal Window", y = expression("90"^th*" percentile ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + 
  theme(panel.border = element_rect(colour = "black",fill=NA, size=1),
        text=element_text(size=12), axis.text=element_text(size=12),axis.title.x = element_blank(),
        axis.title.y=element_text(size=14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "None",plot.margin=unit(c(0.1,0.2,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels=scaleFUN,breaks=c(0,4,8,12,18))+
  annotate(geom="text", x=0.75, y=3.8, label="*",color="black",size=5)+
  annotate(geom="text", x=1.75, y=4.8, label="*",color="black",size=5)+
  annotate(geom="text", x=2.75, y=5.5, label="*",color="black",size=5)+
  annotate(geom="text", x=3.75, y=7, label="*",color="black",size=5)



plot3 = plot3 + coord_cartesian(ylim = c(0,13))
plot3

# Plot 3a annual only
plot3a = ggplot(data=subset(temp_range3,window=='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("90"^th*" percentile ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        text=element_text(size=12), axis.title=element_blank(),
        axis.text=element_text(size=12),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "None",
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey",
                                        size = 0.5, linetype = "solid"),
        plot.margin=unit(c(0.1,0.2,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.14,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  scale_y_continuous(labels=scaleFUN,breaks=c(0,5,10,15,20,25))+
  annotate(geom="text", x=1, y=18, label="*",color="black",size=5)+
  annotate(geom="text", x=0.75, y=15, label="*",color="black",size=5)



plot3a = plot3a + coord_cartesian(ylim = c(0,26))
plot3a


# plot 4

temp_range4 = data.frame(ninetieth_perc = vector(), climate_classification = vector(),
                         window = vector())
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff$daily_ninetieth_perc, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Daily"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff$weekly_ninetieth_perc, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Weekly"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff$biweekly_ninetieth_perc, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Biweekly"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff$monthly_ninetieth_perc, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Monthly"))
temp_range4 = rbind(temp_range4, data.frame(ninetieth_perc = br_diff$annual_ninetieth_perc, 
                                            climate_classification = br_diff$climate_classification,
                                            window = "Annual"))
temp_range4$window = as.factor(temp_range4$window)
temp_range4$window <- factor(temp_range4$window , 
                             levels=c("Daily", "Weekly", "Biweekly","Monthly","Annual"))
temp_range4$climate_classification <- factor(temp_range4$climate_classification , 
                                             levels=c("Tropical", "Subtropical", "Temperate"))

plot4 = ggplot(data=subset(temp_range4,window!='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = expression("90"^th*" percentile ("^o*"C)"),
       fill = "Climate Classification") + theme_bw() + 
  theme(panel.border = element_rect(colour = "black",fill=NA, size=1),
        axis.title = element_text(size=14), axis.text=element_text(size=12),axis.title.x = element_blank(),
        text=element_text(size=12),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = c(0.28, 0.82), legend.spacing.y = unit(0.08, "mm"), legend.title=element_blank(),
        legend.text=element_text(size=8),legend.key.size = unit(0.45, "cm"),
        plot.margin=unit(c(0.1,0.2,0.2,0.2), "cm")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))+
  scale_y_continuous(labels = label_number(accuracy = 0.0001))+
  annotate(geom="text", x=1, y=0.0004, label="*",color="black",size=5)+
  annotate(geom="text", x=0.75, y=0.00035, label="*",color="black",size=5)+
  annotate(geom="text", x=2, y=0.0006, label="*",color="black",size=5)+
  annotate(geom="text", x=1.75, y=0.0005, label="*",color="black",size=5)+
  annotate(geom="text", x=3, y=0.0008, label="*",color="black",size=5)+
  annotate(geom="text", x=2.75, y=0.00055, label="*",color="black",size=5)+
  annotate(geom="text", x=3.75, y=0.00065, label="*",color="black",size=5)+
  annotate(geom="text", x=4, y=0.00094, label="*",color="black",size=5)






# (0.2,0.2,0.3,0.2) = plot (e) margins
plot4 = plot4 + coord_cartesian(ylim = c(0,0.00095))
plot4

# Plot 4a annual only
plot4a = ggplot(data=subset(temp_range4,window=='Annual'), aes(x=window, y=ninetieth_perc, fill=climate_classification))+ 
  geom_rect(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'lightgrey', alpha = 0.5) + 
  geom_boxplot(outlier.shape=NA)+
  labs(x = "Temporal Window", y = "90th percentile",
       fill = "Climate Classification") + theme_bw() + 
  theme(panel.border = element_rect(colour = "black",  fill=NA, size=1),
        axis.title = element_blank(),axis.text=element_text(size=12),
        text=element_text(size=12),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey",size = 0.5, 
                                        linetype = "solid"),
        legend.position = "None",plot.margin=unit(c(0.1,0.7,0.2,0.1), "cm")) +
  scale_fill_manual(values=c("#FC440F", "#E8C547", "#50D8D7")) + 
  annotation_custom(grobTree(textGrob(expression(bold("h")), x=0.14,  y=0.92,
                                      gp=gpar(fontsize=15))))+
  scale_y_continuous(labels = label_number(accuracy = 0.0001))+
  annotate(geom="text", x=1, y=0.0014, label="*",color="black",size=5)

plot4a = plot4a + coord_cartesian(ylim = c(0,0.0015))
plot4a

# EXPORTING FINAL FIG
setwd("~/Desktop/PhD/Publications/Li Chong et al. ocean temp variability")

all_plots = grid.arrange(plot1, plot2,
                         plot3, plot4, ncol = 2)

pdf(width = 12, useDingbats=TRUE,height = 7.55, bg="white", file="Fig_3_JMS_V2") 
all_plots = grid.arrange(plot1, plot2,
                         plot3, plot4, ncol = 2,
                         widths = c(2.7,2.7), heights = c(2.5,2.5))

dev.off()



# save plot (separate annual plots)
ggsave(filename="Fig3_V2_Sept2022.png",height=5, width=11, units="in", plot=grid.arrange(plot1, plot1a, plot2, plot2a,
                                                                                 plot3, plot3a, plot4, plot4a, ncol = 4, 
                                                                                 widths = c(4,1.5, 4,1.7), heights = c(2.5, 2.5)), device="png")

# save plot (separate annual plots); vertically stacked
ggsave(filename="Fig3_V3.png",height=10, width=7, units="in", plot=grid.arrange(plot1, plot1a,
                                                                                  plot3, plot3a, 
                                                                                  plot2, plot2a,
                                                                                  plot4, plot4a, ncol = 2,nrow=4, 
                                                                                  widths = c(4,1.5), heights = c(2.5, 2.1,2.5,2.5)), device="png")

# REMOVED CODE FOR COEFFICIENT OVERLAYS (on top of boxplots)
# + geom_point(data=subset(bayesian_coeffs,X<=15 & window!='Annual'),
#             size=2,position=position_dodge(width=0.75),show_guide = FALSE) + 
#  geom_errorbar(data=subset(bayesian_coeffs,X<=15 & window!='Annual'),
#                aes(ymin=lower.HPD, ymax=upper.HPD),position=position_dodge(width=0.75),width=0.1)

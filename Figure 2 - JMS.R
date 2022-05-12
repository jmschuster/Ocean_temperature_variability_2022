rm(list = ls())
setwd("~/Documents/GitHub/Ocean_temperature_variability_2022/")

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
         labs(title = "Ecuador") + geom_hline(yintercept=28.766,
                                              linetype='dashed')+ 
         geom_hline(yintercept=15.031,linetype='dashed') +
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + 
         annotation_custom(grobTree(textGrob("a", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
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
         labs(title = "Indian Ocean") + geom_hline(yintercept=32.377,
                                                   linetype='dashed')+ 
         geom_hline(yintercept=24.639,linetype='dashed')+
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               text = element_text(size=12)) + 
         annotation_custom(grobTree(textGrob("b", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
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
         labs(title = "Brazil") + geom_hline(yintercept=25.416,
                                             linetype='dashed')+ 
         geom_hline(yintercept=14.517,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               text = element_text(size=12)) + ylab("Temperature (°C)") + 
         annotation_custom(grobTree(textGrob("c", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
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
         labs(title = "Western Australia") + geom_hline(yintercept=31.3002,
                                                linetype='dashed')+ 
         geom_hline(yintercept=20.8347,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               text = element_text(size=12)) + 
         annotation_custom(grobTree(textGrob("d", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
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
         labs(title = "Tasmania") + geom_hline(yintercept=20.793,
                                               linetype='dashed')+ 
         geom_hline(yintercept=8.99,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               text = element_text(size=12)) + ylab("Temperature (°C)") + xlab("Year") + 
         annotation_custom(grobTree(textGrob("e", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
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
         labs(title = "Chile") + geom_hline(yintercept=21.71,
                                            linetype='dashed')+ 
         geom_hline(yintercept=6.54,linetype='dashed')+ 
         theme(plot.title = element_text(hjust = 0.5),
               panel.border = element_rect(colour = "black", fill=NA, size=1),
               axis.title.y = element_blank(),
               text = element_text(size=12)) + xlab("Year")+ scale_x_date(date_breaks='3 month',date_labels = "%b %Y") + 
         annotation_custom(grobTree(textGrob("f", x=0.08,  y=0.92,
                                             gp=gpar(fontsize=15,fontface='bold'))))+ 
         annotation_custom(grobTree(textGrob("Annual minimum", x=0.85,  y=0.09,
                                             gp=gpar(fontsize=10)))) + 
         annotation_custom(grobTree(textGrob("Annual maximum", x=0.85,  y=0.6,
                                             gp=gpar(fontsize=10))))) 


plot6 = plot6 + coord_cartesian(ylim = c(4,35))
plot6
#

grid.arrange(plot1, plot2,  
             plot3, plot4, 
             plot5, plot6, ncol = 2)

# Save final fig: 
setwd("~/Desktop/PhD/Publications/Li Chong et al. ocean temp variability")
ggsave(filename="Fig2_May2022_JMS.png",height=9, width=9, plot=grid.arrange(plot1, plot2,  
                                                        plot3, plot4, 
                                                        plot5, plot6, ncol = 2), device="png")

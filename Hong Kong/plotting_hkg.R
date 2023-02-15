#clear environment
rm(list=ls())

#set directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#set R system language to english
Sys.setenv(LANG = "en")

#load necessary packages
library(ggpubr)
library(tidyverse)
library(readxl)

#import expected mortality for HongKong (produced with "expected_country.R"):
exp.HongKong.avg <- read.table("Data/Data_HongKong_avg")
exp.HongKong.2015 <- read.table("Data/Data_HongKong_2015")
exp.HongKong.2019 <- read.table("Data/Data_HongKong_2019")


#preprocess data for plotting with ggplot
exp.HongKong.avg.grouped <- group_by(exp.HongKong.avg,year)
exp.HongKong.avg.grouped <- summarise(exp.HongKong.avg.grouped,sum(expected))

exp.HongKong.2015.grouped <- group_by(exp.HongKong.2015,year)
exp.HongKong.2015.grouped <- summarise(exp.HongKong.2015.grouped,sum(expected))

exp.HongKong.2019.grouped <- group_by(exp.HongKong.2019,year)
exp.HongKong.2019.grouped <- summarise(exp.HongKong.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(46757, 46662, 45883, 47478, 48706, 50653, 51536)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.HongKong.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.HongKong.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.HongKong.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_HongKong <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Hong Kong") +
    scale_y_continuous(breaks = seq(35000, 65000, 5000),limits = c(38000,62000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_HongKong

#save plot in directory
ggsave("HongKong.png",width = 10,height = 7)


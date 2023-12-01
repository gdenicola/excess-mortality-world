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

#import expected mortality for ch (produced with "expected_country.R"):
exp.ch.avg <- read.table("Data/Data_ch_3y")


#preprocess data for plotting with ggplot
exp.ch.avg.grouped <- group_by(exp.ch.avg,year)
exp.ch.avg.grouped <- summarise(exp.ch.avg.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(67606, 64964, 66971, 67088, 67780, 76195, 71192)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Deaths = exp.ch.avg.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_ch <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Deaths), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Deaths), size = 5,shape=15, col="#619CFF") + 
    #geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - ch") +
  scale_y_continuous(breaks = seq(50000,85000, 5000),limits = c(55000,82000)) +
  scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_ch

#save plot in directory
ggsave("ch_3y.png",width = 10,height = 7)



######## CALCULATING EXCESS MORTALITY #############


## AVERAGE 2017-2019 ##

#Calculate % excess 2020:
(expected_2020 <- df_long$expected[6])
(observed_2020 <- ist.tod[6])
(excess_2020 <- observed_2020 - expected_2020)
(percentage_2020 <- excess_2020 / expected_2020)

#Calculate % excess 2021:
(expected_2021 <- df_long$expected[7])
(observed_2021 <- ist.tod[7])
(excess_2021 <- observed_2021 - expected_2021)
(percentage_2021 <- excess_2021 / expected_2021)

#Calculate % excess overall:
(expected <- expected_2020 + expected_2021)
(observed <- observed_2020 + observed_2021)
(excess <- observed - expected)
(percentage <- excess / expected)

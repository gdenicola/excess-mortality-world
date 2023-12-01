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

#import expected mortality for hrv (produced with "expected_country.R"):
exp.hrv.avg <- read.table("Data/Data_hrv_3y")


#preprocess data for plotting with ggplot
exp.hrv.avg.grouped <- group_by(exp.hrv.avg,year)
exp.hrv.avg.grouped <- summarise(exp.hrv.avg.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(54205, 51542, 53477, 52706, 51794, 57023, 62712)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Deaths = exp.hrv.avg.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_hrv <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Deaths), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Deaths), size = 5,shape=15, col="#619CFF") + 
    #geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - hrv") +
  scale_y_continuous(breaks = seq(40000, 70000, 5000),limits = c(43903.87,65855.8)) +
  scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_hrv

#save plot in directory
ggsave("hrv_3y.png",width = 10,height = 7)



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

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

#import expected mortality for aus (produced with "expected_country.R"):
exp.aus.avg <- read.table("Data/Data_aus_3y")


#preprocess data for plotting with ggplot
exp.aus.avg.grouped <- group_by(exp.aus.avg,year)
exp.aus.avg.grouped <- summarise(exp.aus.avg.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(159052,158504,160909,158493,169301,161300,171469)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Deaths = exp.aus.avg.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_aus <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Deaths), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Deaths), size = 5,shape=15, col="#619CFF") + 
    #geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - aus") +
  scale_y_continuous(breaks = seq(140000,200000, 10000),limits = c(137350,196025)) +
  scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_aus

#save plot in directory
ggsave("aus_3y.png",width = 10,height = 7)



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

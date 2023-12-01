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

#import expected mortality for deu (produced with "expected_country.R"):
exp.deu.avg <- read.table("Data/Data_deu_3y")


#preprocess data for plotting with ggplot
exp.deu.avg.grouped <- group_by(exp.deu.avg,year)
exp.deu.avg.grouped <- summarise(exp.deu.avg.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(925200,910902, 932272, 954874, 939520, 985572, 1023687)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Deaths = exp.deu.avg.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_deu <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Deaths), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Deaths), size = 5,shape=15, col="#619CFF") + 
    #geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - deu") +
    scale_y_continuous(breaks = seq(750000,1150000, 100000),limits = c(750000,1150000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_deu

#save plot in directory
ggsave("deu_3y.png",width = 10,height = 7)



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

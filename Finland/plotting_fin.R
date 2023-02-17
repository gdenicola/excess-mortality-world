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

#import expected mortality for Finland (produced with "expected_country.R"):
exp.Finland.avg <- read.table("Data/Data_Finland_avg")
exp.Finland.2016 <- read.table("Data/Data_Finland_2016")
exp.Finland.2019 <- read.table("Data/Data_Finland_2019")


#preprocess data for plotting with ggplot
exp.Finland.avg.grouped <- group_by(exp.Finland.avg,year)
exp.Finland.avg.grouped <- summarise(exp.Finland.avg.grouped,sum(expected))

exp.Finland.2016.grouped <- group_by(exp.Finland.2016,year)
exp.Finland.2016.grouped <- summarise(exp.Finland.2016.grouped,sum(expected))

exp.Finland.2019.grouped <- group_by(exp.Finland.2019,year)
exp.Finland.2019.grouped <- summarise(exp.Finland.2019.grouped,sum(expected))




#input observed mortality figures by year (2016 to 2021):
ist.tod <- c(52492, 53923, 53722, 54527, 53949, 55488, 57659)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Finland.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Finland.2016.grouped$`sum(expected)`,
                                 exp.lower = exp.Finland.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Finland <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Finland") +
    scale_y_continuous(breaks = seq(40000,70000, 5000),limits = c(43839.6,65759.4)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Finland

#save plot in directory
ggsave("Finland.png",width = 10,height = 7)




######## CALCULATING EXCESS MORTALITY #############

#Calculate % excess 2020:
(expected_2020 <- df_long$expected[16])
(observed_2020 <- ist.tod[6])
(excess_2020 <- observed_2020 - expected_2020)
(percentage_2020 <- excess_2020 / expected_2020)

#Calculate % excess 2021:
(expected_2021 <- df_long$expected[19])
(observed_2021 <- ist.tod[7])
(excess_2021 <- observed_2021 - expected_2021)
(percentage_2021 <- excess_2021 / expected_2021)

#Calculate % excess overall:
(expected <- expected_2020 + expected_2021)
(observed <- observed_2020 + observed_2021)
(excess <- observed - expected)
(percentage <- excess / expected)


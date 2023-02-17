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

#import expected mortality for Japan (produced with "expected_country.R"):
exp.Japan.avg <- read.table("Data/Data_Japan_avg")
exp.Japan.2015 <- read.table("Data/Data_Japan_2015")
exp.Japan.2019 <- read.table("Data/Data_Japan_2019")


#preprocess data for plotting with ggplot
exp.Japan.avg.grouped <- group_by(exp.Japan.avg,year)
exp.Japan.avg.grouped <- summarise(exp.Japan.avg.grouped,sum(expected))

exp.Japan.2015.grouped <- group_by(exp.Japan.2015,year)
exp.Japan.2015.grouped <- summarise(exp.Japan.2015.grouped,sum(expected))

exp.Japan.2019.grouped <- group_by(exp.Japan.2019,year)
exp.Japan.2019.grouped <- summarise(exp.Japan.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(1290510, 1308158, 1340567, 1362470, 1381093, 1372755, 1452289)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Japan.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Japan.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Japan.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Japan <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Japan") +
    scale_y_continuous(breaks = seq(1200000, 1600000, 100000),limits = c(1180000,1590000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Japan

#save plot in directory
ggsave("Japan.png",width = 10,height = 7)




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




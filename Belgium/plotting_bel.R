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

#import expected mortality for Belgium (produced with "expected_country.R"):
exp.Belgium.avg <- read.table("Data/Data_Belgium_avg")
exp.Belgium.2015 <- read.table("Data/Data_Belgium_2015")
exp.Belgium.2019 <- read.table("Data/Data_Belgium_2019")


#preprocess data for plotting with ggplot
exp.Belgium.avg.grouped <- group_by(exp.Belgium.avg,year)
exp.Belgium.avg.grouped <- summarise(exp.Belgium.avg.grouped,sum(expected))

exp.Belgium.2015.grouped <- group_by(exp.Belgium.2015,year)
exp.Belgium.2015.grouped <- summarise(exp.Belgium.2015.grouped,sum(expected))

exp.Belgium.2019.grouped <- group_by(exp.Belgium.2019,year)
exp.Belgium.2019.grouped <- summarise(exp.Belgium.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(110541, 108097, 109666, 110693, 108783, 126896, 112331)


#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Belgium.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Belgium.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Belgium.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Belgium <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Belgium") +
    scale_y_continuous(breaks = seq(90000,136000, 10000),limits = c(90170.68,135256)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Belgium

#save plot in directory
ggsave("Belgium.png",width = 10,height = 7)


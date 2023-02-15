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

#import expected mortality for Germany (produced with "expected_country.R"):
exp.Germany.avg <- read.table("Data/Data_Germany_avg")
exp.Germany.2015 <- read.table("Data/Data_Germany_2015")
exp.Germany.2019 <- read.table("Data/Data_Germany_2019")


#preprocess data for plotting with ggplot
exp.Germany.avg.grouped <- group_by(exp.Germany.avg,year)
exp.Germany.avg.grouped <- summarise(exp.Germany.avg.grouped,sum(expected))

exp.Germany.2015.grouped <- group_by(exp.Germany.2015,year)
exp.Germany.2015.grouped <- summarise(exp.Germany.2015.grouped,sum(expected))

exp.Germany.2019.grouped <- group_by(exp.Germany.2019,year)
exp.Germany.2019.grouped <- summarise(exp.Germany.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(925200,910902, 932272, 954874, 939520, 985572, 1023687)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Germany.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Germany.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Germany.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
p_Germany_2 <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Germany") +
    scale_y_continuous(breaks = seq(750000,1150000, 100000),limits = c(750000,1150000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
p_Germany_2

#save plot in directory
ggsave("Germany.png",width = 10,height = 7)


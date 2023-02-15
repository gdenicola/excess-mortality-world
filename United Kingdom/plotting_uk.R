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

#import expected mortality for UnitedKingdom (produced with "expected_country.R"):
exp.UnitedKingdom.avg <- read.table("Data/Data_UnitedKingdom_avg")
exp.UnitedKingdom.2015 <- read.table("Data/Data_UnitedKingdom_2015")
exp.UnitedKingdom.2019 <- read.table("Data/Data_UnitedKingdom_2019")


#preprocess data for plotting with ggplot
exp.UnitedKingdom.avg.grouped <- group_by(exp.UnitedKingdom.avg,year)
exp.UnitedKingdom.avg.grouped <- summarise(exp.UnitedKingdom.avg.grouped,sum(expected))

exp.UnitedKingdom.2015.grouped <- group_by(exp.UnitedKingdom.2015,year)
exp.UnitedKingdom.2015.grouped <- summarise(exp.UnitedKingdom.2015.grouped,sum(expected))

exp.UnitedKingdom.2019.grouped <- group_by(exp.UnitedKingdom.2019,year)
exp.UnitedKingdom.2019.grouped <- summarise(exp.UnitedKingdom.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(602782, 597206,607172,616014,604707,689629,667479)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.UnitedKingdom.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.UnitedKingdom.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.UnitedKingdom.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_UnitedKingdom <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - UK") +
    scale_y_continuous(breaks = seq(450000,800000, 50000),limits = c(530000,730000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_UnitedKingdom

#save plot in directory
ggsave("UK.png",width = 10,height = 7)


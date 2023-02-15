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

#import expected mortality for Spain (produced with "expected_country.R"):
exp.Spain.avg <- read.table("Data/Data_Spain_avg")
exp.Spain.2015 <- read.table("Data/Data_Spain_2015")
exp.Spain.2019 <- read.table("Data/Data_Spain_2019")


#preprocess data for plotting with ggplot
exp.Spain.avg.grouped <- group_by(exp.Spain.avg,year)
exp.Spain.avg.grouped <- summarise(exp.Spain.avg.grouped,sum(expected))

exp.Spain.2015.grouped <- group_by(exp.Spain.2015,year)
exp.Spain.2015.grouped <- summarise(exp.Spain.2015.grouped,sum(expected))

exp.Spain.2019.grouped <- group_by(exp.Spain.2019,year)
exp.Spain.2019.grouped <- summarise(exp.Spain.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(420408, 408231, 422037, 425153, 416102, 492447, 449270)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Spain.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Spain.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Spain.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Spain <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Spain") +
    scale_y_continuous(breaks = seq(350000,550000, 50000),limits = c(340000,550000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Spain

#save plot in directory
ggsave("Spain.png",width = 10,height = 7)


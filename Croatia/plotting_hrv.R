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

#import expected mortality for Croatia (produced with "expected_country.R"):
exp.Croatia.avg <- read.table("Data/Data_Croatia_avg")
exp.Croatia.2015 <- read.table("Data/Data_Croatia_2015")
exp.Croatia.2019 <- read.table("Data/Data_Croatia_2019")


#preprocess data for plotting with ggplot
exp.Croatia.avg.grouped <- group_by(exp.Croatia.avg,year)
exp.Croatia.avg.grouped <- summarise(exp.Croatia.avg.grouped,sum(expected))

exp.Croatia.2015.grouped <- group_by(exp.Croatia.2015,year)
exp.Croatia.2015.grouped <- summarise(exp.Croatia.2015.grouped,sum(expected))

exp.Croatia.2019.grouped <- group_by(exp.Croatia.2019,year)
exp.Croatia.2019.grouped <- summarise(exp.Croatia.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(54205, 51542, 53477, 52706, 51794, 57023, 62712)
#source: Eurostat


#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Croatia.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Croatia.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Croatia.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Croatia <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Croatia") +
    scale_y_continuous(breaks = seq(40000, 70000, 5000),limits = c(43903.87,65855.8)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Croatia

#save plot in directory
ggsave("Croatia.png",width = 10,height = 7)


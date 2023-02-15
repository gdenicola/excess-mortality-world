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

#import expected mortality for Ireland (produced with "expected_country.R"):
exp.Ireland.avg <- read.table("Data/Data_Ireland_avg")
exp.Ireland.2015 <- read.table("Data/Data_Ireland_2015")
exp.Ireland.2019 <- read.table("Data/Data_Ireland_2019")


#preprocess data for plotting with ggplot
exp.Ireland.avg.grouped <- group_by(exp.Ireland.avg,year)
exp.Ireland.avg.grouped <- summarise(exp.Ireland.avg.grouped,sum(expected))

exp.Ireland.2015.grouped <- group_by(exp.Ireland.2015,year)
exp.Ireland.2015.grouped <- summarise(exp.Ireland.2015.grouped,sum(expected))

exp.Ireland.2019.grouped <- group_by(exp.Ireland.2019,year)
exp.Ireland.2019.grouped <- summarise(exp.Ireland.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(30127, 30667,30317,31140,30959,32387,33058)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Ireland.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Ireland.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Ireland.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Ireland <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Ireland") +
    scale_y_continuous(breaks = seq(25000, 40000, 2500),limits = c(26000, 38000)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Ireland

#save plot in directory
ggsave("Ireland.png",width = 10,height = 7)


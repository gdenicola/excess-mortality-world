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

#import expected mortality for Norway (produced with "expected_country.R"):
exp.Norway.avg <- read.table("Data/Data_Norway_avg")
exp.Norway.2015 <- read.table("Data/Data_Norway_2015")
exp.Norway.2019 <- read.table("Data/Data_Norway_2019")


#preprocess data for plotting with ggplot
exp.Norway.avg.grouped <- group_by(exp.Norway.avg,year)
exp.Norway.avg.grouped <- summarise(exp.Norway.avg.grouped,sum(expected))

exp.Norway.2015.grouped <- group_by(exp.Norway.2015,year)
exp.Norway.2015.grouped <- summarise(exp.Norway.2015.grouped,sum(expected))

exp.Norway.2019.grouped <- group_by(exp.Norway.2019,year)
exp.Norway.2019.grouped <- summarise(exp.Norway.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(40676, 40726, 40774, 40840, 40684, 40611, 42002)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Norway.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Norway.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Norway.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Norway <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Norway") +
    scale_y_continuous(breaks = seq(30000, 55000, 3000),limits = c(32705.6,49058.4)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Norway

#save plot in directory
ggsave("Norway.png",width = 10,height = 7)


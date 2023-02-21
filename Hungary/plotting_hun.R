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

#import expected mortality for Hungary (produced with "expected_country.R"):
exp.Hungary.avg <- read.table("Data/Data_Hungary_avg")
exp.Hungary.2015 <- read.table("Data/Data_Hungary_2015")
exp.Hungary.2019 <- read.table("Data/Data_Hungary_2019")


#preprocess data for plotting with ggplot
exp.Hungary.avg.grouped <- group_by(exp.Hungary.avg,year)
exp.Hungary.avg.grouped <- summarise(exp.Hungary.avg.grouped,sum(expected))

exp.Hungary.2015.grouped <- group_by(exp.Hungary.2015,year)
exp.Hungary.2015.grouped <- summarise(exp.Hungary.2015.grouped,sum(expected))

exp.Hungary.2019.grouped <- group_by(exp.Hungary.2019,year)
exp.Hungary.2019.grouped <- summarise(exp.Hungary.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(131575, 127098, 131877, 131247, 129892, 141326, 156131)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Expected = exp.Hungary.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Hungary.2015.grouped$`sum(expected)`,
                                 exp.lower = exp.Hungary.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Hungary <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Expected), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Expected), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Hungary") +
    scale_y_continuous(breaks = seq(100000,170000, 10000),limits = c(117764.3,163146.4)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Hungary

#save plot in directory
ggsave("Hungary.png",width = 10,height = 7)



######## CALCULATING EXCESS MORTALITY #############


## AVERAGE 2015-2019 ##

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


## LOWER BOUND ##

#Calculate % excess 2020:
(upper_2020 <- df_long$expected[17])
(observed_2020 <- ist.tod[6])
(lower_excess_2020 <- observed_2020 - upper_2020)
(percentage_lower_2020 <- lower_excess_2020 / upper_2020)

#Calculate % excess 2021:
(upper_2021 <- df_long$expected[20])
(observed_2021 <- ist.tod[7])
(lower_excess_2021 <- observed_2021 - upper_2021)
(percentage_lower_2021 <- lower_excess_2021 / upper_2021)

#Calculate % excess overall:
(upper_expected <- upper_2020 + upper_2021)
(observed <- observed_2020 + observed_2021)
(lower_excess <- observed - upper_expected)
(percentage_lower <- lower_excess / upper_expected)


## UPPER BOUND ##

#Calculate % excess 2020:
(lower_2020 <- df_long$expected[18])
(observed_2020 <- ist.tod[6])
(upper_excess_2020 <- observed_2020 - lower_2020)
(percentage_upper_2020 <- upper_excess_2020 / lower_2020)

#Calculate % excess 2021:
(lower_2021 <- df_long$expected[21])
(observed_2021 <- ist.tod[7])
(upper_excess_2021 <- observed_2021 - lower_2021)
(percentage_upper_2021 <- upper_excess_2021 / lower_2021)

#Calculate % excess overall:
(lower_expected <- lower_2020 + lower_2021)
(observed <- observed_2020 + observed_2021)
(upper_excess <- observed - lower_expected)
(percentage_upper <- upper_excess / lower_expected)






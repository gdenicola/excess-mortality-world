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

#import expected mortality for Iceland (produced with "expected_country.R"):
exp.Iceland.avg <- read.table("Data/Data_Iceland_avg")
exp.Iceland.2016 <- read.table("Data/Data_Iceland_2016")
exp.Iceland.2019 <- read.table("Data/Data_Iceland_2019")


#preprocess data for plotting with ggplot
exp.Iceland.avg.grouped <- group_by(exp.Iceland.avg,year)
exp.Iceland.avg.grouped <- summarise(exp.Iceland.avg.grouped,sum(expected))

exp.Iceland.2016.grouped <- group_by(exp.Iceland.2016,year)
exp.Iceland.2016.grouped <- summarise(exp.Iceland.2016.grouped,sum(expected))

exp.Iceland.2019.grouped <- group_by(exp.Iceland.2019,year)
exp.Iceland.2019.grouped <- summarise(exp.Iceland.2019.grouped,sum(expected))




#input observed mortality figures by year (2015 to 2021):
ist.tod <- c(2178, 2309, 2238, 2254, 2275, 2304, 2333)



#build the long-format dataframe for plotting
Year <-  seq(2015,2021,by=1)

df_all_ungrouped <- cbind.data.frame(Year,Deaths = exp.Iceland.avg.grouped$`sum(expected)`,
                                 exp.upper = exp.Iceland.2016.grouped$`sum(expected)`,
                                 exp.lower = exp.Iceland.2019.grouped$`sum(expected)`
            )
df_long <- pivot_longer(df_all_ungrouped,cols = 2:4, 
                        names_to = "type", values_to = "expected")


#produce plot
plot_Iceland <- ggplot(df_all_ungrouped, aes(Year)) + 
    geom_line(aes(y=Deaths), size = 2,linetype = "longdash", col="#619CFF") +
    geom_point(aes(y=Deaths), size = 5,shape=15, col="#619CFF") + 
    geom_ribbon(aes(ymin=exp.upper, ymax=exp.lower), fill="#619CFF", alpha=0.2) +
    geom_point(y=ist.tod,col="black",size = 7) +
    theme_pubr(base_size = 20) +
    ggtitle("Expected versus observed yearly deaths - Iceland") +
    scale_y_continuous(breaks = seq(1500,3000, 250),limits = c(1900,2700)) +
    scale_x_continuous(breaks = seq(2015,2021,1),limits = c(2015,2021)) 

#view plot
plot_Iceland

#save plot in directory
ggsave("Iceland.png",width = 10,height = 7)


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





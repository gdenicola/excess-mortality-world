#clear environment
rm(list=ls())


#set directory to current file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#set R system language to english
Sys.setenv(LANG = "en")

#load necessary packages
library(gdata)
library(readxl)
library(dplyr)

#read in mortality table
lifeTable_avg <- read.xls("Data/mortality_esp_avg.xlsx")
lifeTable_2015 <- read.xls("Data/mortality_esp_2015.xlsx")
lifeTable_2019 <- read.xls("Data/mortality_esp_2019.xlsx")



#create matrix with mortality risks
Hazard_avg <- lifeTable_avg$q.x
Hazard_2015 <- lifeTable_2015$q.x
Hazard_2019 <- lifeTable_2019$q.x



#here we make the adjustment for the average table
Hazard2_avg <- cbind(c(lifeTable_avg$q.x[1],
                   (1/2*(lifeTable_avg$q.x[2:110])+
                      1/2*(lifeTable_avg$q.x[3:111]))))

#here we make the adjustment for the 2015 table
Hazard2_2015 <- cbind(c(lifeTable_2015$q.x[1],
                       (1/2*(lifeTable_2015$q.x[2:110])+
                          1/2*(lifeTable_2015$q.x[3:111]))))

#here we make the adjustment for the 2019 table
Hazard2_2019 <- cbind(c(lifeTable_2019$q.x[1],
                       (1/2*(lifeTable_2019$q.x[2:110])+
                          1/2*(lifeTable_2019$q.x[3:111]))))



#set q.x for 110+ to 1 (in all tables)
Hazard2_avg <- rbind(Hazard2_avg,
                 c(lifeTable_avg$q.x[111]))

Hazard2_2015 <- rbind(Hazard2_2015,
                 c(lifeTable_2015$q.x[111]))

Hazard2_2019 <- rbind(Hazard2_2019,
                 c(lifeTable_2019$q.x[111]))


# 
#    Load and manage population tables 
#

columnnames <- c("year", "age", "both")

#skip to the row of interest and include only the specified number of rows below 
#(only for that year). We have to do this for 2015, 2016, 2017, 2018, 2019, 2020 and 2021. 
#Then bind every resulting data.frame to the next one so that all the information 
#is included in only one data.frame.

data <- read.xls("Data/population_esp.xlsx", header= TRUE, nrows=111 )
data %>% select(1:3)-> data
colnames(data) <- columnnames
#data[,1] <- "2016"

data.2 <- read.xls("Data/population_esp.xlsx", header= TRUE, skip=111, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2017"
data <- rbind(data, data.2)

data.2 <- read.xls("Data/population_esp.xlsx", header= TRUE, skip=222, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2018"
data <- rbind(data, data.2)

data.2 <- read.xls("Data/population_esp.xlsx", header= TRUE, skip=333, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2019"
data <- rbind(data, data.2)

data.2 <- read.xls("Data/population_esp.xlsx", header= TRUE, skip=444, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2020"
data <- rbind(data, data.2)

data.2 <- read.xls("Data/population_esp.xlsx", skip=555, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2021"
data <- rbind(data, data.2)

data.2 <- read.xls("Data/population_esp.xlsx", skip=666, nrows=111 )
data.2 %>% select(1:3)-> data.2
colnames(data.2) <- columnnames
#data.2[,1] <- "2021"
data <- rbind(data, data.2)

#now the dataframe "data" contains population (of both genders) by age and year
head(data)


data %>% select(1, 3) -> data_x

library(data.table)

#data table with just year and population (the age can be inferred by the position)
setDT(data_x)


Hazard2_avg_df <- as.data.frame(Hazard2_avg)
Hazard2_2015_df <- as.data.frame(Hazard2_2015)
Hazard2_2019_df <- as.data.frame(Hazard2_2019)




##### Expected deaths calculation


#Calculation of average expected mortality (2015-2019)

data_x[1:111,2] * Hazard2_avg_df[1:111,1] -> exp_both

data_x[112:222,2] * Hazard2_avg_df[1:111,1] -> exp2016
exp_both <- rbind(exp_both, exp2016)

data_x[223:333,2] * Hazard2_avg_df[1:111,1] -> exp2017
exp_both <- rbind(exp_both, exp2017)

data_x[334:444,2] * Hazard2_avg_df[1:111,1] -> exp2018
exp_both <- rbind(exp_both, exp2018)

data_x[445:555,2] * Hazard2_avg_df[1:111,1] -> exp2019
exp_both <- rbind(exp_both, exp2019)

data_x[556:666,2] * Hazard2_avg_df[1:111,1] -> exp2020
exp_both <- rbind(exp_both, exp2020)

data_x[667:777,2] * Hazard2_avg_df[1:111,1] -> exp2021
exp_both <- rbind(exp_both, exp2021)


# cbind expected deaths to previous data.frame
data_x_new <- cbind(data_x, exp_both)
columnnames3 <- c("year", "population","expected")
colnames(data_x_new) <- columnnames3

#produce data file with expected mortality (will be used for plotting)
write.table(file = "Data/Data_Spain_avg",data_x_new)





#Calculation of upper expected mortality bound (2015 only)

data_x[1:111,2] * Hazard2_2015_df[1:111,1] -> exp_both

data_x[112:222,2] * Hazard2_2015_df[1:111,1] -> exp2016
exp_both <- rbind(exp_both, exp2016)

data_x[223:333,2] * Hazard2_2015_df[1:111,1] -> exp2017
exp_both <- rbind(exp_both, exp2017)

data_x[334:444,2] * Hazard2_2015_df[1:111,1] -> exp2018
exp_both <- rbind(exp_both, exp2018)

data_x[445:555,2] * Hazard2_2015_df[1:111,1] -> exp2019
exp_both <- rbind(exp_both, exp2019)

data_x[556:666,2] * Hazard2_2015_df[1:111,1] -> exp2020
exp_both <- rbind(exp_both, exp2020)

data_x[667:777,2] * Hazard2_2015_df[1:111,1] -> exp2021
exp_both <- rbind(exp_both, exp2021)


# cbind expected deaths to previous data.frame

data_x_new <- cbind(data_x, exp_both)
columnnames3 <- c("year", "population","expected")
colnames(data_x_new) <- columnnames3


#produce data file with expected mortality (will be used for plotting)
write.table(file = "Data/Data_Spain_2015",data_x_new)




#Calculation of lower expected mortality bound (2019 only)

data_x[1:111,2] * Hazard2_2019_df[1:111,1] -> exp_both

data_x[112:222,2] * Hazard2_2019_df[1:111,1] -> exp2016
exp_both <- rbind(exp_both, exp2016)

data_x[223:333,2] * Hazard2_2019_df[1:111,1] -> exp2017
exp_both <- rbind(exp_both, exp2017)

data_x[334:444,2] * Hazard2_2019_df[1:111,1] -> exp2018
exp_both <- rbind(exp_both, exp2018)

data_x[445:555,2] * Hazard2_2019_df[1:111,1] -> exp2019
exp_both <- rbind(exp_both, exp2019)

data_x[556:666,2] * Hazard2_2019_df[1:111,1] -> exp2020
exp_both <- rbind(exp_both, exp2020)

data_x[667:777,2] * Hazard2_2019_df[1:111,1] -> exp2021
exp_both <- rbind(exp_both, exp2021)


# cbind expected deaths to previous data.frame

data_x_new <- cbind(data_x, exp_both)
columnnames3 <- c("year", "population","expected")
colnames(data_x_new) <- columnnames3


#produce data file with expected mortality (will be used for plotting)
write.table(file = "Data/Data_Spain_2019",data_x_new)


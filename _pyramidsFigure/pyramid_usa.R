#code to produce Figure 1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to current


rm(list=ls()) # clear environment
options(scipen=999)


#load necessary packages
library(tidyverse)
library(ggpubr)
library(readxl)


###############2015##################################

# load data

data <- read_excel("usa.xlsx")
long_data <- pivot_longer(data,cols = 3:4,names_to = "Gender",values_to = "Population")
long_data <- mutate(long_data, Population = Population/1000)

# change male population to negative
long_data <- mutate(long_data,
                    Population = ifelse(Gender=="Male", Population*(-1),
                                        Population*1))

#define vector of colours for the bars
colours <- c("tomato1","skyblue")

ggplot(long_data, aes(x = Age,y = Population, fill=Gender)) +
  geom_bar(data = long_data %>% filter(Year == 2020),stat = "identity") +
  geom_step(data = long_data %>% filter(Year == 2015),stat = "identity") +
  coord_flip() +
  theme_pubclean(base_size = 25) +
  scale_fill_manual(values= colours) +
  scale_y_continuous(breaks = seq(-2500,2500, 1000), limits = c(-2580,2580), labels= abs) +
  scale_x_continuous(breaks = seq(0,110,10), position = "top") +
  labs(title = NULL, x = "",
       y = "Population (thousands)") + 
  annotate(geom="text", y=-1300, x=53.2, label="Males 2020",
           color="royalblue4", fontface=2, size=6.5) +
  annotate(geom="text", y=1300, x=53.2, label="Females 2020",
           color="darkred", fontface=2, size=6.5) +
  annotate(geom="text", y=2480, x=20, label="2015",
           color="black", fontface=2, size=6.5) +
  annotate(geom="text", y=-2580, x=20, label="2015",
           color="black", fontface=2, size=6.5) +
  theme(legend.position = "none")

ggsave("_pyramidUSA.pdf",width = 8,height = 10)


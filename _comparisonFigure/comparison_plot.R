setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to current

library(readxl)
library(tidyverse)
library(ggpubr)

rm(list=ls()) # clear environment

#load table from excel
comparison_table <- read_excel("comparison_table.xlsx")

#order table as in paper
comparison_table <- comparison_table[order(comparison_table$`De Nicola & Kauermann`),]

#format table and variables for ggplot
table_long <- pivot_longer(comparison_table,cols = 2:7, names_to = "Method", values_to = "Excess")
table_long$Method <- factor(table_long$Method, levels = c("De Nicola & Kauermann", "Levitt et al.", "Economist", "IHME", "WMD", "WHO"))
table_long$Country <- factor(table_long$Country, levels = comparison_table$Country)



#define vector of colours for the bars
colours <- c("darkorange","brown3", "chartreuse3", "goldenrod1", "skyblue1","blue3")

#produce plot
ggplot(table_long, aes(fct_rev(Country), Excess)) +   
  geom_bar(aes(fill = Method), position = "dodge", stat="identity") +
  coord_flip() + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_pubclean(base_size = 19) +
  theme(legend.key.size = unit(1, 'cm')) +
  xlab("Country") +
  ylab("Excess Mortality") +
  scale_fill_manual(values= colours) +
  scale_y_continuous(labels = scales::percent, breaks = c(-0.15,-0.10, -0.05, 0.00, 0.05, 0.10, 0.15, 0.20, 0.25)) 

 ggsave("_comparison.pdf",width = 10, height = 14.5)






#code to produce Figure 1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set directory to current


rm(list=ls()) # clear environment


#load necessary packages
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

excess_table <- read_excel("excess_table.xlsx")

#get world map
world <- ne_countries(scale = "small", returnclass = "sf") #for stylyzed map
#world <- ne_countries(scale = "medium", returnclass = "sf") #for realistic map

world <- as.data.frame(world)


#rename to match names in "world"

#for scale = small
excess_table$Country[27] <- "United Kingdom"
excess_table$Country[28] <- "United States of America"
excess_table$Country[7] <- "Czech Republic"


#for scale = medium
# excess_table$Country[27] <- "United Kingdom"
# excess_table$Country[28] <- "United States"
# excess_table$Country[7] <- "Czech Rep."
# excess_table$Country[23] <- "Korea"


#remove Hong Kong
excess_table <- excess_table[-12,]

#add column for excess
world$excess <- rep( NA , length(world$sovereignt))

#match values
index <- match(excess_table$Country,world$name_en) # for scale = "small"
#index <- match(excess_table$Country,world$name) # for scale = "medium"

#include values
world$excess[index] <- excess_table$`%Excess`


europe <- world[world$continent=="Europe"&world$admin!="Russia",]



# Graphical visualization:
  ggplot(data = europe, aes(geometry = geometry)) +
  geom_sf(aes(fill = excess)) +
  coord_sf(xlim = c(-25,41), ylim = c(33, 72), expand = FALSE) +
  scale_y_continuous(labels = scales::percent ) +
  theme_classic() +
  scale_fill_gradient2("Excess %", limits = c(-0.25, +0.25), labels = percent, high = "brown3", low = "deepskyblue3") +
  #ggtitle("Excess mortality in Europe, 2020-2021",) +
  #labs(caption = "Analysis and visualization: \n Giacomo De Nicola and Göran Kauermann \n Department of Statistics, LMU Munich") +
  theme(plot.title = element_text(hjust = 0.5,size = 20)) +
  theme(
          legend.key.height = unit(1, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=18), #change legend title font size
          legend.text = element_text(size=14)) +#change legend text font size
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank()) 



ggsave("_map_stylyzed.pdf", width = 11, height = 6, units = "in") #save as pdf


  


####MAX CODE

plot_global <- function(data,
                        upper_limit = NULL,
                        coords_x = c(-30, 50),
                        coords_y = c(30, 70),
                        title = paste0("Excess mortality, years 2020-2021"),
                        border_color = "azure3") {
  
  checkmate::assert_data_frame(data)
  checkmate::assert_numeric(upper_limit, len = 1, null.ok = TRUE)
  checkmate::assert_numeric(coords_x, len = 2)
  checkmate::assert_numeric(coords_y, len = 2)
  
    
    legend_title <- "Total Cases\nper 100.000"
  
  
  # ggplot theme:
  theme <- theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.text.align = 0,
          panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
  
  
  ###IMPORTANT
  
  # Graphical visualization:
  gg <- ggplot(data = world, aes(geometry = geometry))
  # response type "relative"
    gg <- gg + geom_sf(aes(fill = excess))
  
  gg + 
    scale_fill_gradient("A_TITLE", low = "azure2",
                        high = "brown2", trans = "log2", na.value = "white",
                        # breaks = floor(2^(seq(0,log2(floor(upper_limit)+1), length.out = 6))),
                        # labels = floor(2^(seq(0,log2(floor(upper_limit)+1), length.out = 6)))-1,
                        limits = c(0,1)) +
    labs(title = title,
         caption = "Population Estimates: UN 2020\nCase numbers: Johns Hopkins CSSE Data Repository\nVisualization: Statistical Consulting Unit StaBLab, LMU Munich") +
    coord_sf(xlim = coords_x, ylim = coords_y, expand = FALSE) #cutting away the islands
}

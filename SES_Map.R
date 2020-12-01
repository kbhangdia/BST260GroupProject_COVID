library(usmap)
library(ggplot2)
library(readr)
library(lubridate)
library(maps)
library(dplyr)
library(dslabs)
library(stringr)

AllCounty <- map_data("county")
AllCounty %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "red", fill = NA, size = .1 )

clean_df$county = tolower(clean_df$county)
AllCounty <- AllCounty %>% rename("county" = "subregion")

AllCounty = left_join(AllCounty, clean_df, by= "county")


AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = cases)) + 
  geom_polygon(color = "black") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) 
  
library(usmap)
library(ggplot2)
library(readr)
library(lubridate)
library(maps)
library(dplyr)
library(dslabs)
library(stringr)

library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
install.packages("lintr")
library(lintr) # code linting
install.packages("sf")
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
install.packages("viridis")
library(viridis) # viridis color scale
install.packages("cowplot")
library(cowplot) # stack ggplots
library(rmarkdown)
library(ggthemes)
install.packages("ggalt")
library(ggalt)
install.packages("biscale")
library(biscale)

library(cowplot)
library(reshape2)
library(viridis)
library(RColorBrewer)

#new variable for %of population >65 years 
clean_df_updated_quantiles <- clean_df_updated_quantiles %>%
  mutate(percent_over_65 = (clean_df_updated_quantiles$total_over_65 / clean_df_updated_quantiles$total_population)*100)

#three groups of the variables for the bivariate map 
summary(clean_df_updated_quantiles$cases_per_1000ppl)
case_rate_quantile<- quantile(clean_df_updated_quantiles$cases_per_1000ppl,c(0.33,0.66,1), na.rm = TRUE)
poverty_rate_quantile<- quantile(clean_df_updated_quantiles$percent_below_poverty_level,c(0.33,0.66,1), na.rm = TRUE)
high_mask_usage_quantile <-quantile(clean_df_updated_quantiles$high_mask_usage_sum,c(0.33,0.66,1), na.rm = TRUE)
household_size_quantile <-quantile(clean_df_updated_quantiles$avg_household_size,c(0.33,0.66,1), na.rm = TRUE)
percent_over_65_quantile <-quantile(clean_df_updated_quantiles$percent_over_65,c(0.33,0.66,1), na.rm = TRUE)
pop_density_quantile <-quantile(clean_df_updated_quantiles$pop_density_sq_km,c(0.33,0.66,1), na.rm = TRUE)
worked_home_quantile<-quantile(clean_df_updated_quantiles$homeoffice_per_1000ppl,c(0.33,0.66,1), na.rm = TRUE)

#categorical variable 1-3 to represent the three quantiles 
clean_df_updated_quantiles<- clean_df_updated_quantiles %>% mutate(
                               y= ifelse(percent_below_poverty_level<poverty_rate_quantile[1],1,ifelse(percent_below_poverty_level<poverty_rate_quantile[2],2,3)) ,
                               x= ifelse(cases_per_1000ppl<case_rate_quantile[1],1,ifelse(cases_per_1000ppl<case_rate_quantile[2],2,3)),
                               z= ifelse(high_mask_usage_sum<high_mask_usage_quantile[1],1,ifelse(high_mask_usage_sum<high_mask_usage_quantile[2],2,3)),
                               a= ifelse(avg_household_size<household_size_quantile[1],1,ifelse(avg_household_size<household_size_quantile[2],2,3)),
                               b= ifelse(percent_over_65<percent_over_65_quantile[1],1,ifelse(percent_over_65<percent_over_65_quantile[2],2,3)),
                               c= ifelse(pop_density_sq_km<pop_density_quantile[1],1,ifelse(pop_density_sq_km<pop_density_quantile[2],2,3)),
                               d= ifelse(homeoffice_per_1000ppl<worked_home_quantile[1],1,ifelse(homeoffice_per_1000ppl<worked_home_quantile[2],2,3))
                               )  
#transform the indicator variables to be numeric 
clean_df_updated_quantiles$x = as.numeric(clean_df_updated_quantiles$x)
clean_df_updated_quantiles$y = as.numeric(clean_df_updated_quantiles$y)
clean_df_updated_quantiles$z = as.numeric(clean_df_updated_quantiles$z)
clean_df_updated_quantiles$a = as.numeric(clean_df_updated_quantiles$a)
clean_df_updated_quantiles$b = as.numeric(clean_df_updated_quantiles$b)
clean_df_updated_quantiles$c = as.numeric(clean_df_updated_quantiles$c)
clean_df_updated_quantiles$d = as.numeric(clean_df_updated_quantiles$d)

#single variable for covid case rate and poverty 
clean_df_updated_quantiles$bivariate <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$y==1, 1, 
                                               ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$y==1, 2, 
                                                      ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$y==1, 3,
                                                             ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$y==2, 4,
                                                                    ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$y==2, 5,
                                                                           ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$y==2, 6,
                                                                                  ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$y==3, 7,
                                                                                         ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$y==3, 8,
                                                                                                ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$y==3, 9, 
                                                                                                       FALSE)))))))))

#single variable for covid case rate and mask use
clean_df_updated_quantiles$bivariate_mask <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$z==1, 1, 
                                                    ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$z==1, 2, 
                                                           ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$z==1, 3,
                                                                  ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$z==2, 4,
                                                                         ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$z==2, 5,
                                                                                ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$z==2, 6,
                                                                                       ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$z==3, 7,
                                                                                              ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$z==3, 8,
                                                                                                     ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$z==3, 9, 
                                                                                                            FALSE)))))))))
#single variable for covid case rate and household size 
clean_df_updated_quantiles$bivariate_household <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$a==1, 1, 
                                                    ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$a==1, 2, 
                                                           ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$a==1, 3,
                                                                  ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$a==2, 4,
                                                                         ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$a==2, 5,
                                                                                ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$a==2, 6,
                                                                                       ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$a==3, 7,
                                                                                              ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$a==3, 8,
                                                                                                     ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$a==3, 9, 
                                                                                                            FALSE)))))))))

#single variable for covid case rate and percent over 65 
clean_df_updated_quantiles$bivariate_over_65 <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$b==1, 1, 
                                                         ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$b==1, 2, 
                                                                ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$b==1, 3,
                                                                       ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$b==2, 4,
                                                                              ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$b==2, 5,
                                                                                     ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$b==2, 6,
                                                                                            ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$b==3, 7,
                                                                                                   ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$b==3, 8,
                                                                                                          ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$b==3, 9, 
                                                                                                                 FALSE)))))))))

#single variable for covid case rate and population density 
clean_df_updated_quantiles$bivariate_pop_density <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$c==1, 1, 
                                                       ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$c==1, 2, 
                                                              ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$c==1, 3,
                                                                     ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$c==2, 4,
                                                                            ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$c==2, 5,
                                                                                   ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$c==2, 6,
                                                                                          ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$c==3, 7,
                                                                                                 ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$c==3, 8,
                                                                                                        ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$c==3, 9, 
                                                                                                               FALSE)))))))))

#single variable for covid case rate and worked from home 
clean_df_updated_quantiles$bivariate_worked_home <- ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$d==1, 1, 
                                                           ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$d==1, 2, 
                                                                  ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$d==1, 3,
                                                                         ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$d==2, 4,
                                                                                ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$d==2, 5,
                                                                                       ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$d==2, 6,
                                                                                              ifelse(clean_df_updated_quantiles$x==1 & clean_df_updated_quantiles$d==3, 7,
                                                                                                     ifelse(clean_df_updated_quantiles$x==2 & clean_df_updated_quantiles$d==3, 8,
                                                                                                            ifelse(clean_df_updated_quantiles$x==3 & clean_df_updated_quantiles$d==3, 9, 
                                                                                                                   FALSE)))))))))

#loading US county map data and plotting base map 
AllCounty <- map_data("county")
AllCounty %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "red", fill = NA, size = .1 )

#wrangling data to remove lowecase and renaming 
clean_df_updated_quantiles$county = tolower(clean_df_updated_quantiles$county)
AllCounty <- AllCounty %>% rename("county" = "subregion")

#merging map data with 
AllCounty = left_join(AllCounty, clean_df_updated_quantiles, by= "county")

#making all bivariate variables factor variables 
AllCounty$bivariate <- as.factor(AllCounty$bivariate)
AllCounty$bivariate_mask <- as.factor(AllCounty$bivariate_mask)
AllCounty$bivariate_household <- as.factor(AllCounty$bivariate_household)
AllCounty$bivariate_over_65 <- as.factor(AllCounty$bivariate_over_65)
AllCounty$bivariate_pop_density <- as.factor(AllCounty$bivariate_pop_density)
AllCounty$bivariate_worked_home <- as.factor(AllCounty$bivariate_worked_home)

#testing out with basic map 
AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = cases_per_1000ppl)) + 
  geom_polygon(color = "NA") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) 


#bivarate map of cases and poverty 
AllCounty$bivariate <- as.factor(AllCounty$bivariate)
map_poverty <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
    panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and poverty in the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_poverty <-map_poverty + scale_fill_manual(values = cbp1)
map_poverty 


#legend for map of cases and poverty
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_poverty<- test 
lg_poverty<-lg_poverty + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_poverty<-lg_poverty + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing Poverty-->")
lg_poverty<-lg_poverty+theme(axis.text=element_blank())
lg_poverty<-lg_poverty+theme(line=element_blank())
lg_poverty

#map plus legend for cases and poverty 
ggdraw() +
  draw_plot(map_poverty, 0, 0, 1, 1) +
  draw_plot(lg_poverty, 0.05, 0.075, 0.25, 0.25)

#################################
#bivarate map of cases and mask use 
map_mask <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate_mask)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and mask use in the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_mask <-map_mask + scale_fill_manual(values = cbp1)
#map_mask 


#legend for map of cases and mask use
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_mask<- test 
lg_mask<-lg_mask + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_mask<-lg_mask + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing mask use-->")
lg_mask<-lg_mask+theme(axis.text=element_blank())
lg_mask<-lg_mask+theme(line=element_blank())
lg_mask

#map plus legend for cases and mask use  
ggdraw() +
  draw_plot(map_mask, 0, 0, 1, 1) +
  draw_plot(lg_mask, 0.05, 0.075, 0.25, 0.25)

#################################
#bivarate map of cases and household size 
map_household <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate_household)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and household size in the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_household <-map_household + scale_fill_manual(values = cbp1)
map_household 


#legend for map of cases and household size 
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_household<- test 
lg_household<-lg_household + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_household<-lg_household + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing household size-->")
lg_household<-lg_household+theme(axis.text=element_blank())
lg_household<-lg_household+theme(line=element_blank())
lg_household

#map plus legend for cases and household size 
ggdraw() +
  draw_plot(map_household, 0, 0, 1, 1) +
  draw_plot(lg_household, 0.05, 0.075, 0.25, 0.25)

#################################
#bivarate map of cases and % over 65
map_65 <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate_over_65)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and  % over 65 in the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_65 <-map_65 + scale_fill_manual(values = cbp1)
map_65 


#legend for map of cases and % over 65 
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_65<- test 
lg_65<-lg_65 + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_65<-lg_65 + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing percent over 65 years-->")
lg_65<-lg_65+theme(axis.text=element_blank())
lg_65<-lg_65+theme(line=element_blank())


#map plus legend for cases and % over 65
ggdraw() +
  draw_plot(map_65, 0, 0, 1, 1) +
  draw_plot(lg_65, 0.05, 0.075, 0.25, 0.25)

#################################
#bivarate map of cases and pop density 
map_density <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate_pop_density)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and population density the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_density <-map_density + scale_fill_manual(values = cbp1)
map_density 


#legend for map of cases and population density 
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_density<- test 
lg_density<-lg_density + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_density<-lg_density + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing population density-->")
lg_density<-lg_density+theme(axis.text=element_blank())
lg_density<-lg_density+theme(line=element_blank())


#map plus legend for cases and population density 
ggdraw() +
  draw_plot(map_density, 0, 0, 1, 1) +
  draw_plot(lg_density, 0.05, 0.075, 0.25, 0.25)


#################################
#bivarate map of cases and worked from home 
map_home <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate_worked_home)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) +
  labs(
    title = "Covid 19 rates and % working from home (WFH) the US",
    subtitle = "bivariate choropleth map")

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map_home <-map_home + scale_fill_manual(values = cbp1)
map_home 


#legend for map of cases and worked from home
melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")

lg_home<- test 
lg_home<-lg_home + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg_home<-lg_home + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing WFH-->")
lg_home<-lg_home+theme(axis.text=element_blank())
lg_home<-lg_home+theme(line=element_blank())


#map plus legend for cases and worked from home 
ggdraw() +
  draw_plot(map_home, 0, 0, 1, 1) +
  draw_plot(lg_home, 0.05, 0.075, 0.25, 0.25)


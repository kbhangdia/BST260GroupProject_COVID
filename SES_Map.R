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


#three groups of the variables for the bivariate map 
summary(clean_df_updated$cases_per_1000ppl)
case_rate_quantile<- quantile(clean_df_updated$cases_per_1000ppl,c(0.33,0.66,1), na.rm = TRUE)
poverty_rate_quantile<- quantile(clean_df_updated$percent_below_poverty_level,c(0.33,0.66,1), na.rm = TRUE)

clean_df_updated_quantiles<- clean_df_updated %>% mutate(y= ifelse(percent_below_poverty_level<poverty_rate_quantile[1],1,ifelse(percent_below_poverty_level<poverty_rate_quantile[2],2,3)) ,
                               x= ifelse(cases_per_1000ppl<case_rate_quantile[1],1,ifelse(cases_per_1000ppl<case_rate_quantile[2],2,3))  )  

clean_df_updated_quantiles$x = as.numeric(clean_df_updated_quantiles$x)
clean_df_updated_quantiles$y = as.numeric(clean_df_updated_quantiles$y)

ggplot(data=clean_df_updated_quantiles,aes(x=cases_per_1000ppl,y=percent_below_poverty_level,color=atan(y/x),alpha=x+y))+
  geom_point(size=1)+  guides(alpha=F,color=F)+
  geom_hline(yintercept=poverty_rate_quantile,color="gray20",linetype=2)+
  geom_vline(xintercept=case_rate_quantile,color="gray20",linetype=2)+
  scale_color_viridis(name="Color scale")+theme_minimal()+
  theme(plot.caption=element_text(size = 9, hjust=0),
        panel.grid=element_blank()) +
  
  labs(x="Total COVID 19 cases per 1,000 population on 11/30/20",
       y="Percent of population living below the poverty line",
       caption="Source: U.S. Census Bureau\nEach dot one county, lines divide (univariate) terciles")+
  # limit the rang e
  scale_x_continuous(breaks=c(case_rate_quantile),
                     labels=round(c(case_rate_quantile),2)) +
  scale_y_continuous(breaks=c(poverty_rate_quantile),
                     labels=round(c(poverty_rate_quantile),2)) 

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


#loading US county map data and plotting base map 
AllCounty <- map_data("county")
AllCounty %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "red", fill = NA, size = .1 )

#wrangling data to remove lowecase and renaming 
clean_df_updated_quantiles$county = tolower(clean_df_updated_quantiles$county)
AllCounty <- AllCounty %>% rename("county" = "subregion")

#merging map data with 
AllCounty = left_join(AllCounty, clean_df_updated_quantiles, by= "county")


AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = cases_per_1000ppl)) + 
  geom_polygon(color = "NA") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) 

#dont look at this 
data <- bi_class(AllCounty, x = case_rate, y = percent_below_poverty_level, style = "quantile", dim = 3)
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Race and Income in St. Louis, MO",
    subtitle = "Dark Blue (DkBlue) Palette"
  ) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher % White ",
                    ylab = "Higher Income ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)


#this might be working 
library(viridis)
library(RColorBrewer)
AllCounty$bivariate <- as.factor(AllCounty$bivariate)
map <- AllCounty %>% ggplot(aes(x = long, y = lat, group = group, fill = bivariate)) + 
  geom_polygon(color = "NA") +
  theme(legend.position = "None",
    panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_fixed(1.3) 

cbp1 <- c("#E8E8E8", "#ACE4E4", "#5AC8C8", "#DFB0D6",
          "#A5ADD3", "#5698B9", "#BE64AC", "#8C62AA", "#3B4994")
map <-map + scale_fill_manual(values = cbp1)
map
#test desired legend appearance
library(ggplot2)
library(cowplot)
library(reshape2)

melt(matrix(1:9,nrow=3))
legendGoal=melt(matrix(1:9,nrow=3))
test<-ggplot(legendGoal, aes(Var2,Var1,fill = as.factor(value)))+ geom_tile()
test<- test + scale_fill_manual(name="",values=cbp1)
test<-test + theme(legend.position="none")
#test<-ggdraw(test) + draw_text(text = "More Var 2 -->",x=0.91,y=0.58)
#test<-ggdraw(test) + draw_text(text = "More Var 1 -->",x=0.84,y=0.5,angle=270)

#create a plot that will be the legend itself
lg<- test #would not be true when making a map
lg<-lg + theme(axis.title.x=element_text(size=rel(.60),color=bvColors[3])) + xlab("Increasing COVID rates -->")
lg<-lg + theme(axis.title.y=element_text(size=rel(.60),color=bvColors[3])) + ylab("Increasing Poverty-->")
lg<-lg+theme(axis.text=element_blank())
lg<-lg+theme(line=element_blank())
lg
#put both plots on a grid


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(lg, 0.05, 0.075, 0.2, 0.2)

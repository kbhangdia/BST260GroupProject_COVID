library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
install.packages("maps")
library(maps)

#Below is the code for our data cleaning / wrangling 


#first we renamed the variables 
Worked_at_Home_Counties <- Worked_at_Home_Counties %>% rename("total_pop_worked_home" = "DP03_0024E")
Race_and_Ethnicity_County <- Race_and_Ethnicity_County %>% rename("White" = "B02001_002E",
                                                                  "Black" = "B02001_003E",
                                                                  "American_Indian_Alaska_Native" = "B02001_004E",
                                                                  "Asian" = "B02001_005E",
                                                                  "Native_Hawaiian_Pacific_Islander" = "B02001_006E",
                                                                  "Other" = "B02001_007E",
                                                                  "Two_or_more" = "B02001_008E",
                                                                  "Not_Hispanic_Latino" = "B03001_002E",
                                                                  "Hispanic_Latino" = "B03001_003E")
Population_and_Poverty_Counties <- Population_and_Poverty_Counties %>% rename("percent_below_poverty_level" = "HOUSEBELOWPOVP_CALC")

Population_by_Age_and_Sex_Counties <- Population_by_Age_and_Sex_Counties %>% rename("total_population" = "B01001_001E")
Population_by_Age_and_Sex_Counties <- Population_by_Age_and_Sex_Counties %>% rename("total_households_w_someone_over_65" = "DP02_0014E",
                                                                                    "total_over_65" = "DP05_0029E")
Average_Household_Size_and_Population_Density_County <- Average_Household_Size_and_Population_Density_County %>% rename("avg_household_size" = "B25010_001E",
                                                                                                                        "pop_density_sq_km" = "B01001_calc_PopDensity") 
Average_Household_Size_and_Population_Density_County <- Average_Household_Size_and_Population_Density_County %>% rename("OBJECTID" = "FID") 
Health_Insurance_Coverage_Counties <- Health_Insurance_Coverage_Counties %>% rename("percent_no_health_insurance" = "DP03_0099PE")


#then we subsetted and kept only the variables of interest
Worked_at_Home_Counties_clean = subset(Worked_at_Home_Counties, select = c("OBJECTID", "total_pop_worked_home"), drop = FALSE)
Health_Insurance_Coverage_Counties_clean = subset(Health_Insurance_Coverage_Counties, select = c("OBJECTID", "percent_no_health_insurance"), drop = FALSE)
Average_Household_Size_and_Population_Density_County_clean = subset(Average_Household_Size_and_Population_Density_County, select = c("OBJECTID", "GEOID", "NAME", "State", "avg_household_size", "pop_density_sq_km"), drop = FALSE)
Population_and_Poverty_Counties_clean = subset(Population_and_Poverty_Counties, select = c("OBJECTID", "percent_below_poverty_level"), drop = FALSE)
Population_by_Age_and_Sex_Counties_clean = subset(Population_by_Age_and_Sex_Counties,  select = c("OBJECTID", "total_population", "total_households_w_someone_over_65", "total_over_65"), drop = FALSE)
Race_and_Ethnicity_County_clean = subset(Race_and_Ethnicity_County, select = c("OBJECTID",
                                                                               "White",  
                                                                               "Black",
                                                                               "American_Indian_Alaska_Native",
                                                                               "Asian",
                                                                               "Native_Hawaiian_Pacific_Islander",
                                                                               "Other",
                                                                               "Two_or_more",
                                                                               "Not_Hispanic_Latino",
                                                                               "Hispanic_Latino"),
                                                                               drop = FALSE) 
#then we used a left join to merge the data sets into one data frame called clean_df basedo on the objectids of each county
clean_df = left_join(Worked_at_Home_Counties_clean, Health_Insurance_Coverage_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Average_Household_Size_and_Population_Density_County_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Population_and_Poverty_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Population_by_Age_and_Sex_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Race_and_Ethnicity_County_clean, by= "OBJECTID")

#then we loaded US counties data for mapping and renamed the variable ID 
us_counties <- us_counties %>% rename("GEOID" = "fips")

#then we joined the map data with our dataframe with all variables of interest 
clean_df = left_join(clean_df, us_counties, by= "GEOID")

#renamed mask use 
mask_use_by_county <- mask_use_by_county %>% rename("GEOID" = "COUNTYFP")

#and merged it 
clean_df = left_join(clean_df, mask_use_by_county, by= "GEOID")

# Creating a 'cases per 1000 ppl' indicator
clean_df <- clean_df %>%
  mutate(cases_per_1000ppl = ((clean_df$cases / clean_df$total_population) * 1000))
head(clean_df)
# There are some observations that report "cases per 1000 ppl" > 1000. For the purposes of our analyses, we will drop these observations. 
clean_df <- clean_df %>%
  filter(cases_per_1000ppl < 1000)
# We dropped 120 observations

# Creating a 'worked from home per 1000 ppl' indicator
clean_df <- clean_df %>%
  mutate(homeoffice_per_1000ppl = ((clean_df$total_pop_worked_home / clean_df$total_population) * 1000))
head(clean_df)

# Creating a 'high mask usage' indicator
clean_df <- clean_df %>%
  mutate(high_mask_usage_sum = (clean_df$ALWAYS + clean_df$FREQUENTLY))
clean_df$high_mask_usage <- ifelse(clean_df$high_mask_usage_sum > 0.6, 1, 0)
head(clean_df)

clean_df_updated=clean_df
#saving this df as a cvs file 
write.csv(clean_df_updated, file= "/Users/kayleighbhangdia/Desktop/BST260GroupProject_COVID/clean_df_updated.csv")





library(ggplot2)
library(tidyverse)
library(lubridate)
library(zoo)
install.packages("maps")
library(maps)

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

clean_df = left_join(Worked_at_Home_Counties_clean, Health_Insurance_Coverage_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Average_Household_Size_and_Population_Density_County_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Population_and_Poverty_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Population_by_Age_and_Sex_Counties_clean, by= "OBJECTID")
clean_df = left_join(clean_df, Race_and_Ethnicity_County_clean, by= "OBJECTID")

us_counties <- us_counties %>% rename("GEOID" = "fips")

clean_df = left_join(clean_df, us_counties, by= "GEOID")

mask_use_by_county <- mask_use_by_county %>% rename("GEOID" = "COUNTYFP")

clean_df = left_join(clean_df, mask_use_by_county, by= "GEOID")

write.csv(clean_df, file= "/Users/kayleighbhangdia/Desktop/BST260GroupProject_COVID/clean_df.csv")

?write.csv

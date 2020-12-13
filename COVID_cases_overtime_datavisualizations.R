library(tidyverse)
library(lubridate)
library(zoo)
library(maps)
library(dplyr)

#Renaming Datafile
USCOVID <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time 
colnames(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time)

#Converting Character to Date
str(USCOVID$submission_date)
USCOVID$submission_date <- mdy(USCOVID$submission_date)
str(USCOVID$submission_date) # check date Date

USpolicies$Date <- mdy(USpolicies$Date)

#Line plot
state_list <- c("CA", "FL", "NYC") 

temp <- USCOVID %>% filter(state %in% state_list)
ggplot(temp, aes(x = submission_date, y = tot_cases, colour = state)) + 
  geom_line(size = 1) +
  labs(title = "Covid-19 Total Confirmed Cases by State", x = "Submission Date", y = "Total Number of Cases") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
temp

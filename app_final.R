#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(maps)
library(dplyr)

library(readr)
United_States_COVID_19_Cases_and_Deaths_by_State_over_Time <- read_csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")
USpolicies <- read_csv("USpolicies.csv")

#Renaming Datafile
USCOVID <- United_States_COVID_19_Cases_and_Deaths_by_State_over_Time 
colnames(United_States_COVID_19_Cases_and_Deaths_by_State_over_Time)

#Converting Character to Date
str(USCOVID$submission_date)
USCOVID$submission_date <- mdy(USCOVID$submission_date)
str(USCOVID$submission_date) # check date Date

USpolicies$Date <- mdy(USpolicies$Date)

date_line <- USpolicies %>% filter(Policy == "Election") %>% pull(Date)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Cases Over Time for Selected States"),

    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "dates",
                label= "Select policy:",
                choices = (USpolicies$Policy),
                selected = "Election"),
    
    selectInput(inputId = "state",
                label= "Select state:",
                choices = unique(USCOVID$state),
                multiple = TRUE,
                selected = "FL"),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )

# Define server logic required to draw a line plot
server <- function(input, output) {
    output$distPlot <- renderPlot({
        date_line <- USpolicies %>% filter(Policy == input$dates) %>% pull(Date)
        
        USCOVID %>% filter(state %in% input$state) %>%
        ggplot(aes(x = submission_date, y = tot_cases, colour = state)) + 
        geom_vline(xintercept = date_line) + 
        geom_line(size = 1) +
        labs(title = "Covid-19 Total Confirmed Cases by State", x = "Submission Date", y = "Total Number of Cases", input$state) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

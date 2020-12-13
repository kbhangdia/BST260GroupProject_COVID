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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Cases Over Time for Selected States"),

    # Sidebar with a slider input for number of bins 
    selectInput(inputId = "dates",
                label= "Select date:",
                choices = (USpolicies$Date),
                selected = 2020-07-04))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        state_list <- c("CA", "FL", "NYC") 
        
        temp <- USCOVID %>% filter(state %in% state_list)
        ggplot(temp, aes(x = submission_date, y = tot_cases, colour = state)) + 
            geom_line(size = 1) +
            labs(title = "Covid-19 Total Confirmed Cases by State", x = "Submission Date", y = "Total Number of Cases") +
            theme_classic() +
            geom_vline(xintercept = as.Date.character((USpolicies$Date)))
            theme(plot.title = element_text(hjust = 0.5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

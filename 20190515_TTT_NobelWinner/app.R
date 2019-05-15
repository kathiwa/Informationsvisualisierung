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
library(ggplot2)
library(DT)

#nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

#write_rds(nobel_winners,"nobelwin.rds")

nobel_winners <- read_rds("nobelwin.rds")

df <- nobel_winners [,c(-3:-10,-14:-18)]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Nobel Prize Winner"),
   
   # Sidebar with a slider input for number of bins 
   
      
      # Show a plot of the generated distribution
     
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)


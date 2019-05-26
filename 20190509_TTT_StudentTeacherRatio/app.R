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
library(DT)
library(ggplot2)

#student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

#write_rds(student_ratio,"studentratio.rds")

student_ratio <- read_rds("studentratio.rds")


df <- student_ratio %>% 
      filter(student_ratio != "NA") %>% 
      mutate(student_ratio = round(student_ratio, digits = 1)) 


country_selection <- df %>% 
                    group_by(country, year) %>% 
                    summarize(totalstudentRatio = sum(student_ratio)) 

# Define UI for application that draws a histogram
ui <- fluidPage(
        
  # Application title
  titlePanel("Student Teacher Ratio"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Select","Country", choices = country_selection$country, multiple = TRUE)
      ),
      
   # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  
  #FluidRow
  fluidRow(
    column(3,
           wellPanel(
             checkboxInput("y12", "2012", FALSE),
             checkboxInput("y13", "2013", FALSE),
             checkboxInput("y14", "2014", FALSE),
             checkboxInput("y15", "2015", FALSE),
             checkboxInput("y16", "2016", FALSE),
             checkboxInput("y17", "2017", FALSE),
             checkboxInput("y18", "2018", FALSE)
           )
    ),
    
    column(7,
           
           # Show a plot of the generated distribution
           plotOutput("rankingplot", brush = "plot_brush"),
           DT::DTOutput("tabledata")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
 # output$distPlot <- renderPlot({
#    country_selection[,c("country"),drop=FALSE]
 #   })

#  output$tabledata <- renderDT(brushedPoints(country_selection, input$plot_brush))
  
    output$rankingplot <- renderPlot({
      temp <- country_selection %>% filter(country == "ÜAWKEÄGJ") %>% filter(year=="oajpoj0")

      if(input$y12){
        temp <- country_selection %>% filter(year == 2012) %>% bind_rows(temp)
      }
      if(input$y13){
        temp <- country_selection %>% filter(year == 2013) %>% bind_rows(temp)
      }
      if(input$y14){
        temp <- country_selection %>% filter(year == 2014) %>% bind_rows(temp)
      }
      if(input$y15){
        temp <- country_selection %>% filter(year == 2015) %>% bind_rows(temp)
      }
      if(input$y16){
        temp <- country_selection %>% filter(year == 2016) %>% bind_rows(temp)
      }
      if(input$y17){
        temp <- country_selection %>% filter(year == 2017) %>% bind_rows(temp)
      }
      if(input$y18){
        temp <- country_selection %>% filter(year == 2018) %>% bind_rows(temp)
      }
      temp <- temp %>% filter(country %in% input$Select) 
      
    temp %>%
      ggplot( 
        aes(country,totalstudentRatio, fill=factor(year),group = factor(year))) + 
      geom_col(position = "dodge2") +
      scale_fill_discrete() +
      labs(x="Countries",
         y="Student Ratio",
         title= "Student Ratio over Year",
         fill = "Jahr")+
      coord_flip()

  })
}  

# Run the application 
shinyApp(ui = ui, server = server)


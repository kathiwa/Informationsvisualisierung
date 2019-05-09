#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("tidyverse")

library(shiny)
library(tidyverse)
library(DT)

#tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")
#write_rds(tidy_anime,"temp.rds")


tidy_anime <- read_rds("temp.rds")

anime <- tidy_anime %>% select(animeID, name, title_english, genre, episodes, rating, score, scored_by, rank,popularity) %>% mutate(rating = factor(rating))

genre_names <- anime$genre %>% unique() %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Anime Dataset"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("select","Lieblingsgenre", choices = genre_names)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"), 
      plotOutput("distPlot")
    )
  ),
  
  #FluidRow
  fluidRow(
    column(3,
           wellPanel(
             checkboxInput("action", "Action", FALSE),
             checkboxInput("adv", "Adventure", FALSE),
             checkboxInput("com", "Comedy", FALSE),
             checkboxInput("drama", "Drama", FALSE),
             checkboxInput("fant", "Fantasy", FALSE),
             checkboxInput("horror", "Horror", FALSE),
             checkboxInput("kids", "Kids", FALSE),
             checkboxInput("rom", "Romance", FALSE),
             checkboxInput("scifi", "Sci-Fi", FALSE),
             checkboxInput("supern", "Supernatural", FALSE)
           )
    ),
    
    column(9,
           # Show a plot of the generated distribution
           plotOutput("rankingplot", brush = "plot_brush"),
           DT::DTOutput("tabledata")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$distPlot <- renderPlot({
  #  ggplot(anime) + aes(x = genre, y = score)+
  #   geom_point() + coord_flip()
  #})
  
  output$tabledata <- renderDT(brushedPoints(anime, input$plot_brush))
  
  
  output$rankingplot <- renderPlot({
    
    temp <- anime %>% filter(genre == "ÜAWKEÄGJ")
    if(input$action){
      temp <- anime %>% filter(genre == "Action")
    }
    if(input$adv){
      temp <- anime %>% filter(genre == "Adventure") %>% bind_rows(temp)
    }
    if(input$com){
      temp <- anime %>% filter(genre == "Comedy") %>% bind_rows(temp)
    }
    if(input$drama){
      temp <- anime %>% filter(genre == "Dinstall.packages('rsconnect')rama") %>% bind_rows(temp)
    }
    if(input$fant){
      temp <- anime %>% filter(genre == "Fantasy") %>% bind_rows(temp)
    }
    if(input$horror){
      temp <- anime %>% filter(genre == "Horror") %>% bind_rows(temp)
    }
    if(input$kids){
      temp <- anime %>% filter(genre == "Kids") %>% bind_rows(temp)
    }
    if(input$rom){
      temp <- anime %>% filter(genre == "Romance") %>% bind_rows(temp)
    }
    if(input$scifi){
      temp <- anime %>% filter(genre == "Sci-Fi") %>% bind_rows(temp)
    }
    if(input$supern){
      temp <- anime %>% filter(genre == "Supernatural") %>% bind_rows(temp)
    }
    
    temp %>% ggplot()+ 
      aes(score, log(episodes), color = genre) + 
      geom_point(alpha = 0.2)+
      labs(x="Score",
           y="Number of Episodes",
           title= "Anime Genre over Score")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
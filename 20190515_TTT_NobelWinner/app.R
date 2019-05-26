library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(hcictools)

#nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")

#write_rds(nobel_winners,"nobelwin.rds")

nobel_winners <- read_rds("nobelwin.rds")

rwthcolor <- hcictools::rwth.colorpalette()

df <- nobel_winners [,c(-3:-10,-14:-18)]

df <- df %>% arrange(birth_country)

# Define UI for application that draws a histogram ----
ui <-  dashboardPage(
        dashboardHeader(title="Nobel Prize Laureates"),
        dashboardSidebar(
          sidebarMenu(
            menuItem("Home Page", tabName="homepage",icon=icon("fas fa-home")),
            menuItem("Gender-Overview",tabName = "gender",icon=icon("chart-area"))
          )
        ),
        dashboardBody(
          tabItems(
            # First tab content ----
            tabItem(tabName = "homepage",
                    fluidRow(
                      titlePanel("Welcome to the World of Nobel Prizes"), 
                      box( "This app provides an overview of all Nobel Prizes Winners since 1901. The tab 'Gender-Overview' will show you a scatterplot of prize categories over birth countries of the winners and years the prize were won, split up by gender.")
                    )
            ),

            # Second tab content ----
            tabItem(tabName = "gender",
                    fluidRow(
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("countselect","Birth Country", choices = df$birth_country, multiple = TRUE, selected = "Germany")
                        ),
                      box(width = "12",
                        title = "Overview of Nobel Prize Winners over Gender",
                        status="primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                          mainPanel(
                            plotOutput("genplot", height= "600px",width = "800px")
                          )
                        )
                      )
                    )
            )
                  
          )
        )
      )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$text <- renderText ({input$text})
  
  
  #Tab2 ----
output$genplot <- renderPlot({
 

df <- df %>% filter(birth_country != "NA") %>% filter(birth_country %in% input$countselect)
  
      ggplot(data= df,
        aes(x = prize_year,y = birth_country, color=category))+
        geom_point()+
        facet_wrap(~gender)+
        theme(legend.position = "bottom",plot.title = element_text(size=15,face = "bold"))+
        labs(x="Year the Nobel Prize was won",
          y="Birth Countries of Nobel Prize Winners",
          color="Categories of Nobel Prizes",
          title="Categories of Nobel Prizes over Birth Countries of the Winners.",
          subtitle="Scatterplot of all Nobel Prizes won between 2001 and 2016 split by gender.")
  })    
}

# Run the application ----
shinyApp(ui = ui, server = server)

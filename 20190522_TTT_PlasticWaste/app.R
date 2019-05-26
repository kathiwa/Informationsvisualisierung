
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

#mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
#write_rds(mismanaged_vs_gdp,"wastefail.rds")
#waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
#write_rds(waste_vs_gdp,"wastetotal.rds")

wastetotal <- read_rds("wastetotal.rds")

wastefail <- read_rds("wastefail.rds")

df_total <- wastetotal %>% filter(`Per capita plastic waste (kilograms per person per day)` != "NA") %>% filter (`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`!= "NA")
df_fail <- wastefail %>% filter(`Per capita mismanaged plastic waste (kilograms per person per day)` !="NA") %>% filter(`GDP per capita, PPP (constant 2011 international $) (Rate)` !="NA")


df <- bind_cols(df_fail,df_total)

df <- df[,c(-7:-10,-12)]

# Define UI for application that draws a histogram
ui <- dashboardPage(
      dashboardHeader(title = "Global Plastic Waste"),
      dashboardSidebar(
        #Menü einrichten----
        sidebarMenu(
          menuItem("Home Page",tabName="homepage",icon=icon("fas fa-globe-europe")),
          menuItem("Plastic Waste",tabName="plasticwaste",icon=icon("gulp")),
          menuItem("Plastic Waste over GDP",tabName="compare",icon=icon("chart-line"))
        )
      ),
      dashboardBody(
        tabItems(
          #First tab content---- 
          tabItem(tabName = "homepage",
            box(
              title = "Challenges for the Future",
              status = "warning",
              "Plastic pollution is a major and growing problem, negatively affecting oceans and wildlife health. This app provides an overview of Mismanaged Plastic Waste of 148 countries. Furthermore, it discusses whether there is a correlation between the amount of plastic that is produced and the GDP of each country.")
          )
        ),
          #Second tab content ----
      fluidRow(
        infoBoxOutput("progressBox"),
          tabItems(  
          tabItem(tabName = "plasticwaste",
                  fluidRow(
                    box(width="12",
                      title="Mismanaged Plastic Waste",
                      box(plotOutput("wasteplot"))
                    )
                  )
          )
        )  
      ),
          #Third tab content----
        tabItems(  
          tabItem(tabName = "compare",
                fluidRow(
                  box(width="12",
                      title="Mismanaged Plastic Waste",
                      plotOutput("compplot")
                  )
                )
          )      
        ) 
      )
)
#Server----
server <- function(input, output) { 
  output$text <- renderText({input$text})
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Mismanaged Waste", paste0(25 + input$count, "%"), icon = icon("trash"),
      color = "purple"
    )
  })
  
#  output$menuitem <- renderMenu ({
#    menuItem("homepage",icon=icon("fas fa-globe-europe"))
#  })
#  output$menuitem <- renderMenu ({
#    menuItem("plasticwaste",icon=icon("gulp"))
#  })
#  output$menuitem <- renderMenu ({
#    menuItem("compare",icon=icon("chart-line"))
#  })
  
  output$wasteplot <- renderPlot({
    
    ggplot(data= df,
           aes(x = ,y = ))+
      geom_point()+
      theme(legend.position = "bottom",plot.title = element_text(size=15,face = "bold"))+
      labs(x="",
           y="",
           title="")
  })  
  
  output$compplot <- renderPlot({
    
    ggplot(data= df_total,
           aes(x = 'Per capita plastic waste (kilograms per person per day)', y =  ))+
      geom_point()+
      theme(legend.position = "bottom",plot.title = element_text(size=15,face = "bold"))+
      labs(x="",
           y="",
           title="")
  })  
  
}
shinyApp(ui, server)
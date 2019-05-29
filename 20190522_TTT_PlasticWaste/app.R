library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plyr)
library(codebook)

  #Load Data and Data cleaning ----

#mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
#write_rds(mismanaged_vs_gdp,"wastefail.rds")
#waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
#write_rds(waste_vs_gdp,"wastetotal.rds")
wastetotal <- read_rds("wastetotal.rds")
wastefail <- read_rds("wastefail.rds")
df_total <- wastetotal %>% filter(`Per capita plastic waste (kilograms per person per day)` != "NA") %>% filter (`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`!= "NA")
#df_fail <- wastefail %>% filter(`Per capita mismanaged plastic waste (kilograms per person per day)` !="NA") %>% filter(`GDP per capita, PPP (constant 2011 international $) (Rate)` !="NA")
#df <- bind_rows(df_fail,df_total)
#write.table(df_clean, file = "wastefinal.csv", sep = ";")

#wastefail2 <- wastefail %>% filter(`Per capita mismanaged plastic waste (kilograms per person per day)`!= "NA") %>% mutate(totalwaste = `Per capita mismanaged plastic waste (kilograms per person per day)`*`Total population (Gapminder)`)
#sum(wastefail2$totalwaste, na.rm = TRUE)

wastetotal2 <- wastetotal %>% filter(`Per capita plastic waste (kilograms per person per day)`!= "NA") %>% mutate(totalwaste = `Per capita plastic waste (kilograms per person per day)`*`Total population (Gapminder)`)
sum(wastetotal2$totalwaste, na.rm = TRUE)

#df_clean <- read.csv(file="waste.csv", header=TRUE, sep=";")
df_new <- df_clean %>% rename(c('Per.capita.mismanaged.plastic.waste..kilograms.per.person.per.day.' = "waste", 'GDP.per.capita..PPP..constant.2011.international.....constant.2011.international...' = "gdp", 'Total.population..Gapminder.' = "totalPop", 'Waste.total.or.missmanaged' = "waste_type"))

df_clean <- df_new %>% mutate(wasteYear = 365*waste) 

#UI ----

# Define UI for application that draws a histogram
ui <- dashboardPage(
      dashboardHeader(title = "Global Plastic Waste"),
      dashboardSidebar(
        width = 250,
              #Menü einrichten----
        sidebarMenu(
          menuItem("Home Page",tabName = "homepage",icon=icon("fas fa-globe")),
          menuItem("Plastic Waste",tabName = "plasticwaste",icon=icon("gulp")),
          menuItem("Plastic Waste over GDP",tabName = "compare",icon=icon("chart-line"))
        )  
      ),

      dashboardBody(
        tabItems(
        #First tab content---- 
        tabItem(tabName = "homepage",
         fluidRow(
           infoBoxOutput("totalbox", width=6), 
           infoBoxOutput("percentbox", width = 6),
            
           box(width = 12,
              title = "Challenges for the Future",
              status = "warning",
              "Plastic pollution is a major and growing problem, negatively affecting oceans and wildlife health. This app provides an overview of Mismanaged Plastic Waste of 148 countries in 2010. Furthermore, it discusses whether there is a correlation between the amount of plastic that is produced and the GDP of each country.")
          )
        ),
      
            #Second tab content ----
        tabItem(tabName = "plasticwaste",
                  fluidRow(
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("countselect","Select a Country", choices = df_clean$Entity, multiple = TRUE, selected = "Germany")
                      ),
                    box(width="12",
                      title="Total Waste vs Missmanaged Waste",
                      plotOutput("wasteplot")
                    )
                    )
          )            
        ),
    

          #Third tab content----
        tabItem(tabName = "compare",
                fluidRow(
                      box(width="12",
                      title="Waste over GDP",
                      plotOutput("compplot")
                      )
                )
        )      
      )
      
    )
)


#Server----
server <- function(input, output) { 

#Output Menu ----
  output$menu <- renderMenu ({
    sidebarMenu(
      menuItem("Home Page",tabName = "homepage",icon=icon("fas fa-globe")),
      menuItem("Plastic Waste",tabName = "plasticwaste",icon=icon("gulp")),
      menuItem("Plastic Waste over GDP",tabName = "compare",icon=icon("chart-line"))
    )  
  })

#Output Text Homepage----
  output$text <- renderText({input$text})

#Output Progess Box----      
  output$percentbox <- renderInfoBox({
    infoBox(
      "Missmanaged Waste", paste0(25 + input$count,"39,4%"), icon = icon("percent"),
      color = "purple"
    )
  })
  
  output$totalbox <- renderInfoBox({
    infoBox(
      "Total Waste per day", paste0(25 + input$count, "749.684t"), icon = icon("trash"),
      color = "red"
    )
  })
  
#Output Plot Tab2 ----
output$wasteplot <- renderPlot({
    
temp <- df_clean %>% 
        filter(Entity %in% input$countselect)

      ggplot(temp, 
        aes(x = Entity, y = wasteYear, fill= waste_type, group_by(waste_type))) + 
      geom_col(position = "dodge2") +
      labs(x="Countries",
           y="Quantity of waste per Person per Year for all Countries",
           title= "Comparison of total amount of Plastic Waste and mismanaged Plastic Waste over Countries",
           fill = "Total vs Missmanaged Waste per Person per Year")+
        coord_flip()
})
  
#Output Plot Tab3----  
  output$compplot <- renderPlot({
    
         ggplot(df_total)+
           aes(x = log(`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`), y =(`Per capita plastic waste (kilograms per person per day)`))+
      theme(legend.position = "bottom",plot.title = element_text(size=15,face = "bold"))+
      labs(x="Rate of GDP per capita in thousand US-Dollars (2011)",
           y="Plastic Waste per capita in 2010 [kg per day]",
           title="Plastic Waste over GDP per Capita",
           subtitle="Scatterplot shows that")+
      geom_point()+
      scale_y_log10()
  })  
  
}
shinyApp(ui, server)
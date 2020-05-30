#LOAD R PACKAGES
library(shiny)
library(shinythemes)

# UI

ui <- fluidPage(theme=shinytheme("superhero"),
                
                titlePanel("Lab Exercise 1: Association Rule Mining/Recommender System"),
                
                sidebarLayout(
                  
                  sidebarPanel(), 
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Page 1"), 
                      tabPanel("Page 2"), 
                      tabPanel("Page 3")
                    )
                  )
                )
)

# SERVER
server <- function(input, output, session) { }

#LAUNCH SHINY OBJECT
shinyApp(ui = ui, server = server)

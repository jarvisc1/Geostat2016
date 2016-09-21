#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sp)
library(mapview)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Resolution (cell size, m):",
                     min = 1,
                     max = 50,
                     value = 30),
         selectInput("parent",
                     "Parent soil type:",
                     choices = c("a", "b"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Use a separate observer to recreate the legend as needed.
        leafletOutput("mymap"),
        mapview:::plainViewOutput("test")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  p = readRDS("../dat/p.Rds")
  m <- mapview(p)
  output$mymap <- renderLeaflet({
    m@map
  })
  
})

# Run the application 
shinyApp(ui, server)

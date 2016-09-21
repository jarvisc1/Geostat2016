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
library(raster)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Predicting soil type using spatial and attribute data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Resolution (cell size, m):",
                     min = 100,
                     max = 1000,
                     step = 100, 
                     value = 500),
         selectInput("parent",
                     "Parent soil type:",
                     choices = c("a", "b"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Use a separate observer to recreate the legend as needed.
        leafletOutput("m")
        # mapview:::plainViewOutput("test")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  initial_lat = 0.2081755
  initial_lon = 25.331 
  
  p = readRDS("training.Rds")
  v = readRDS("v.Rds")
  r = readRDS("raster-mini.Rds")

  # m <- mapview(r) + mapview(p)
  output$m = renderLeaflet({
    
    if(input$bins > 100){
      r = aggregate(r, input$bins / 100)
    }
    
    projections = c("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") # Africa_Albers_Equal_Area_Conic 
    aproj = projections
    crs(r) = crs(p) = crs(v) = aproj
    p = spTransform(p, CRS("+init=epsg:4326"))
    v = spTransform(v, CRS("+init=epsg:4326"))
    r = projectRaster(r, crs = "+init=epsg:4326")
    
    r_sub = r$DEMNED6_100m
    
    leaflet() %>%
      addCircles(data = p) %>%
      addCircles(data = v, color = "grey") %>%
      addRasterImage(r_sub) %>%
      setView(lng = initial_lon, lat = initial_lat, zoom = 11)
  })
  
})

# Run the application 
shinyApp(ui, server)

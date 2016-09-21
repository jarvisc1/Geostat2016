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
ui = shinyUI(fluidPage(
  fluidRow(
    column(2,
           "Controls",
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
    column(10,
           "Interactive map",
           leafletOutput("m", width ="100%", height = "800")
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
    
    pal = colorFactor(palette = "RdYlBu", domain = unique(p$TAXNUSDA))
    
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
      addCircles(data = v, color = ~pal(p$TAXNUSDA)) %>%
      addRasterImage(r_sub) %>%
      setView(lng = initial_lon, lat = initial_lat, zoom = 11) %>% 
      addLegend(pal = pal, values = p$TAXNUSDA)
  })
  
})

# Run the application 
shinyApp(ui, server)

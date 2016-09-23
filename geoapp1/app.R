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
library(deldir)
library(dismo)
library(caret)
# Define UI for application that draws a histogram
ui = shinyUI(fluidPage(
  fluidRow(
    titlePanel(title = "GEOSTAT 2016 Game Entry: interactive visualisation understand spatial models"),
    column(3,
           "Controls",
           tabsetPanel(
             tabPanel("Raster data",
           sliderInput("bins",
                       "Resolution (cell size, m):",
                       min = 100,
                       max = 1000,
                       step = 100, 
                       value = 500),
           selectInput("raster_layer",
                       "Raster layer:",
                       choices = c(
                         "Elevation (m)" = "DEMNED6_100m",
                         "Land cover map" = "LNDCOV6_100m",
                         "Parent materials" = "PMTGSS7_100m",
                         "MODIS EVI image (EX1MOD5)"= "EX1MOD5_100m",
                          "Precipitation (cm/yr)" = "Precip"
                                   )),
           selectInput("raster_pal", "Colourscheme:",
                       choices = c(
                         "terrain.colors",
                         "heat.colors",
                         "topo.colors",
                         "cm.colors",
                         "rainbow"
                       )
                       )
           ),
           tabPanel("Point data",
                    selectInput("point_layer",
                                "Point visualisation method:",
                                choices = c(
                                  "Circles" = "c",
                                  "Voronoi polygons" = "v"
                                  )
                                ),
                    sliderInput(inputId = "circle_size", label = "Predicted values circle size:", min = 10, max = 1000, value = 200, step = 10),
                    conditionalPanel(condition = "input.point_layer == 'c'",
                                     sliderInput(inputId = "pcircle_size", label = "Oberved values circle size:", min = 10, max = 1000, value = 200, step = 10)
                                     )
                       )
           ),
           hr(),
           fluidRow("  ", 
                    selectInput(inputId = "model", label = "Model selection:", choices = c("None", "Random", "Voronoi", "Altitude-dependent", "Random Forest"))
           )
           # ,
           # hr(),
           # fluidRow(" ",
           #   textOutput()
           # )
           ),
    column(9,
           "Interactive map",
           leafletOutput("m", width ="100%", height = "800"),
           actionButton("reset_button", "Reset view")
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  initial_lat = 0.2081755
  initial_lon = 25.331 
  initial_zoom = 12
  
  p = readRDS("training.Rds")
  v = readRDS("v.Rds")
  r = readRDS("raster-mini.Rds")
  sel_precip = grep(pattern = "PR", x = names(r))
  r$Precip = sum(r[[sel_precip]])

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
    vo = dismo::voronoi(p)
    v$TAXNUSDA = NA
    if(input$model=="Voronoi"){
      v$TAXNUSDA <- raster::extract(vo, v)$TAXNUSDA
    }
    if(input$model == "Random"){
      v$TAXNUSDA <- sample(unique(p$TAXNUSDA), size = nrow(v), replace = T)
    }
    if(input$model == "Altitude-dependent"){
      height = as.data.frame(extract(r, v))$DEMNED6_100m
      height_training = as.data.frame(extract(r, p))$DEMNED6_100m
      closest_height = sapply(height, function(x) which.min(abs(x - height_training)))
      v$TAXNUSDA <- p$TAXNUSDA[closest_height]
      # aggregate(height ~ v$TAXNUSDA, FUN = mean) # test model
      # aggregate(height_training ~ p$TAXNUSDA, FUN = mean) # test model
    }    
    if(input$model == "Random Forest"){
      v$TAXNUSDA <- v$pred
    }
    r_sub = r[[input$raster_layer]]
    raster_pal = match.fun(input$raster_pal)
    hide_v = ifelse(input$point_layer == "v", "nv", "v")
    
    leaflet() %>%
      addRasterImage(r_sub, raster_pal(n = 10)) %>%
      addCircles(data = p, color = ~pal(p$TAXNUSDA), radius = input$pcircle_size, opacity = 1) %>%
        addPolygons(data = vo, fillColor = ~pal(vo$TAXNUSDA), fillOpacity = 1, group = "v" ) %>% 
      hideGroup(hide_v) %>% 
      addCircles(data = v, color = ~pal(v$TAXNUSDA), radius = input$circle_size) %>%
      addLegend(pal = pal, values = p$TAXNUSDA, title = "Soil type") %>%
      mapOptions(zoomToLimits = "first")  
    
  })
  
  observe({
    input$reset_button
    leafletProxy("m") %>% setView(lat = initial_lat, lng = initial_lon, zoom = initial_zoom)
  })
  
})

# Run the application 
shinyApp(ui, server)

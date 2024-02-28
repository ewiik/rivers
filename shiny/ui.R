library(shiny)
library(leaflet)

ui <- navbarPage("Llygredd",
  
  # Main panel for displaying outputs
    tabPanel("Map",
      # Output: Leaflet map
      leafletOutput("map", width = "100%", height = "80vh")),
      
  
      # Output: Data attributions
      tabPanel("Data attributions",
               htmlOutput('attr')
    )
  )

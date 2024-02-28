## hepful on observe vs reactive https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
## if not two maps but want one where new features become uncovered: https://stackoverflow.com/questions/32216819/zooming-into-state-to-view-zipcode-using-r-leaflet
## hot to track memory in R (might become useful to test):http://adv-r.had.co.nz/memory.html
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% 
      addTiles() %>%
      setView(lng = -4.129263, lat =  53.227390 , zoom = 10) %>%
      addProviderTiles(providers$OpenTopoMap, options=providerTileOptions(minZoom=11)) %>% 
      addProviderTiles("CartoDB.Positron", options=providerTileOptions(maxZoom=10, apikey=apithunder)) %>% # , options = providerTileOptions(opacity = 0)
      addPolygons(data=prot, fillColor=~protpal(Designation), color=~protpal(Designation), weight=2,
                  fillOpacity = .7, opacity=.9, label=prot$Name, smoothFactor = 5,
                  highlightOptions = highlightOptions(bringToFront = T, color="blue", 
                                               weight=4),
                  group='Protected sites') %>%
      addPolygons(data=sac, fillColor=~sacpal(statusbinned), fillOpacity = 0.4, color='black', weight = 3, 
                 label=sac$Name, smoothFactor = 3,
              highlight = highlightOptions(bringToFront = T, stroke=T, color="black", weight=3, fill=F),
               group = 'SAC WFD sections') %>%
      addPolylines(data=wfd, color=~wfdpal(Status), weight = 3, opacity = 0.5, smoothFactor = 3,
                  highlight = highlightOptions(bringToFront = T, stroke=T, color="black", fill=F),
                 group = 'WFD rivers') %>% # halo line next (can't create in same call)
      addPolylines(data=wfd, color=~wfdpal(Status), weight = 7, opacity = 0.2, label = wfd$Name, smoothFactor = 3,
                  highlight = highlightOptions(bringToFront = T, stroke=T, color="black", fill=F),
                 group = 'WFD rivers') %>% #'#762a83'
      addCircleMarkers(data=usk, radius = ~radiuz*3,fillColor = ~perpal(Permit), 
                       fillOpacity = 0.6, color = "black", weight = 2, group="Outflows",
                       label = ~paste(SEWER_FUNC,":",TDV..m3.day.,"m3/day", sep="")) %>%
      addCircleMarkers(data=wye, radius = ~radiuz*3,fillColor = ~perpal(Permit), 
                       fillOpacity = 0.6, color = "black", weight = 2, group="Outflows",
                       label=~paste(SEWER_FUNC,":",TDV..m3.day.,"m3/day", sep="")) %>%
      addCircleMarkers(data=perms,fillColor = ~sewpal(release), radius=5,
                      fillOpacity = 1, color = "black", weight = 3, group="Outflows",
                     label=~paste(release, permit_number, sep = ':')) %>%
      addLayersControl(
        #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("Protected sites",'WFD rivers',"SAC WFD sections", "Outflows"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Protected sites", 'WFD rivers',"SAC WFD sections","Outflows")) %>% # default setting
      addLegend("bottomleft", pal = protpal, values=prot$Designation, # https://github.com/rstudio/leaflet/issues/485
                title = "Designation",group='Protected sites',
                #labFormat = labelFormat(prefix = "$"),
                opacity = 0.8)  %>%
      addLegend("bottomleft", pal = sacpal, title = "P(ug/L) from target",
        values = sac$statusbinned, opacity = 0.9, group="SAC WFD sections") %>%
      addLegend("bottomleft", pal = wfdpal, title = "WFD status",
                values = wfd$Status, opacity = 0.9) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'black'
                                                                              ,weight = 5)),
        circleMarkerOptions = FALSE,
        circleOptions = FALSE,
        editOptions = editToolbarOptions())
  })
  
  ## data attributions
  output$attr <- renderUI({
    #theurl <- a("Who Owns England", href="http://map.whoownsengland.org/")
    theurl2 <- a("Welsh Government Portal", href="https://datamap.gov.wales/")
    tagList(theurl2)
  }) 
}

# library(rsconnect)
# rsconnect::deployApp('path/to/your/app')

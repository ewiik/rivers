## plot basic River Wye stuff.
## clip shreve and any other relevant layers to the catchments

## read in files that will be used for clipping
ceh <- rgdal::readOGR("../../SAC rivers/SpatialFiles/CEH-Catchments50k", layer="CEHCatchments50k", stringsAsFactors = F) # from Arc layers
ceh <- spChFIDs(ceh, as.character(1:length(ceh))) # IMPORTANT!! for indexing need to start at 1 not 0

shh <- rgdal::readOGR("../../SAC rivers/SpatialFiles/CEH-Shreve", layer="CEH50kShreve", stringsAsFactors = F) # ibid
shh <- spChFIDs(shh, as.character(1:length(shh))) # IMPORTANT!! for indexing need to start at 1 not 0

sac <- rgdal::readOGR("../../SAC rivers/SpatialFiles/SpecialAreasOfConservationSAC", layer="NRW_SACPolygon",
                              stringsAsFactors = F)
sac <- spChFIDs(sac, as.character(1:length(sac))) # IMPORTANT!! for indexing need to start at 1 not 0

#obs <- rgdal::readOGR("../RiverObstructions", layer="RiverObstructions", stringsAsFactors = F) # ibid

bound <- rgdal::readOGR("../../SAC rivers/SpatialFiles/Counties", 
                        layer="Counties_and_Unitary_Authorities__December_2016__Boundaries")
parks <- rgdal::readOGR("../../SAC rivers/SpatialFiles/NationalParks", layer="NRW_NATIONAL_PARKPolygon", 
                        stringsAsFactors = F)
nations <- rgdal::readOGR("../../SAC rivers/SpatialFiles/Countries", layer="Countries__December_2017__Boundaries", 
                          stringsAsFactors = F)

## ==================================================================================
## create some important functions to use later
## ========================================================================
listN <- function(...){ # function that automagically creates named list using objects provided
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

## IMPORTANT!!! uses row indices so rownames absolutely have to be checked:
##    if default row numbers, then works as index number. if not... trouble!
## this also means to check if row starts at 0 or 1. needs to start at 1 for indexing.
## e.g. spatiallines and polygons start at 0. so ID slot (slot(obj, "polygons")[[1]])
##    for first entry returns 0. so then, for selecting, need to alter.
## or getSpPPolygonsIDSlots(ceh)[1]
getWithins <- function(spframe, cwant) {
  want <- gWithin(spframe, cwant, byid=T, returnDense = F) # gives rowname of rivers in cwant
  want <- Filter(Negate(is.null), want) # returns those true with rowname for which river
  want <- as.data.frame(do.call(rbind, want))
  want$index <- rownames(want)
  want <- split(want, f=want$V1)
  wantsubs <- lapply(want, function(x) {inds <- as.numeric(x$index); x <- spframe[inds,]})
  return(wantsubs)
}

## =========================================================================
## change CRS of files that need it
## ==========================================================================
crslist <- listN(ceh, shh)
crslist <- lapply(crslist, function(x) {x <- spTransform(x, CRS("+init=epsg:4326"))})

list2env(crslist, envir = globalenv())
rm(crslist)

crslist <- listN(bound, parks, nations)
crslist <- lapply(crslist, function(x) {x <- spTransform(x, CRS("+init=epsg:4326"))})

list2env(crslist, envir = globalenv())
rm(crslist)


## ================================================================================
## grab rivers from the CEH 50k layer and create zones!
## ==============================================================================
wye <- ceh[grep("wye", tolower(ceh$NAME)),] 
wye <- rbind(wye, ceh[ceh$NAME %in% c("Irfon C055010","Ithon C055012","Lugg C055005"),])

# comparing tywi and usk geodatabase to CEH: need....
#tywi <- ceh[ceh$FEATURE %in% c("196660","196661","196659","196658","196657","196655"),]
#usk <- ceh[ceh$FEATURE %in% c("196633","196634","196637","196636"),]

#rm(ceh) # save space in working tree

wyeu <- gUnaryUnion(wye)
#cleddu <- SpatialPolygonsDataFrame(cleddu, data=data.frame(Short="Cleddau"))

## =========================================================================
## subset national data to appropriate catchments
## ===============================================================================
test <- lapply(list(shh, sac), getWithins, cwant=wyeu) 
names(test) <- c('shhcrop', 'saccrop')
list2env(test,globalenv()) # return the crops into the working environment

## do on shreves (note this list structure inherits from other script)
shhcrop <- shhcrop[[1]]
saccrop <- saccrop[[1]]
wyesac <- sac[grep("wye", tolower(sac$SAC_name)),] # FIXME: the CEH thing doesn't actually encompass the
#   entire wye sac catchment so something is wrong here.and the CEH thing is poss wrong completeyl. Totes
#   dumb that there isn't a column showing which main river system the subcatchments correspond to
saccrop <- rbind(saccrop, wyesac)

## ==============================================================================
## start plotting
## ===========================================================================
provwant <-  "Thunderforest.Outdoors"
provwant <- "CartoDB.Positron"

# palette for shreve
shhcrop$SHREVE <- as.numeric(shhcrop$SHREVE)
shhcrop$shrevetrunc <- shhcrop$SHREVE
shhcrop$shrevetrunc[shhcrop$shrevetrunc>6] <- 6

shhpal <- colorNumeric(
  palette = "Blues",
  domain = shhcrop$shrevetrunc)


leaflet() %>%
  addMapPane(name = "polygons", zIndex = 410) %>% 
  #addMapPane(name = "maplabels", zIndex = 400) %>% # higher zIndex rendered on top, archaic from labelsonly layer
  addProviderTiles(provwant,options = providerTileOptions(apikey = apithunder)) %>% #"") %>%
  addPolygons(data=bound, fill=F, color="black", weight=1, dashArray = "1",
              fillOpacity = 0, opacity=0.5, group='counties', options=leafletOptions(pane = "polygons")) %>% 
  addPolygons(data=parks, fill="#C4CDC1", color="#C4CDC1", weight=1,
              fillOpacity = 0.4, opacity=0.2, group='counties', options=leafletOptions(pane = "polygons")) %>% 
  addPolygons(data=saccrop, fillColor="blue", color="blue", weight=1.5,#dashArray = "1,5",
              fillOpacity = 0.5, opacity=1, group='mylayers', options=leafletOptions(pane = "polygons")) %>%
  addPolylines(data=shhcrop, fill=F, stroke = T, color=~shhpal(shrevetrunc), weight=0.6,
               fillOpacity = 0.2, opacity=1, group='mylayers', options=leafletOptions(pane = 'polygons')) %>% 
  addLayersControl(baseGroups = provwant,
                   overlayGroups = c("mylayers")) %>%
  #addLegend("bottomleft", colors = sacpal(sac$Short), opacity = 1,
   #         labels = c(paste(sac$SAC_name, sac$Site_code, sep=":")),title = "SAC full names") %>% # https://github.com/rstudio/leaflet/issues/485
  addScaleBar(position = c("bottomright"), options = scaleBarOptions(imperial=F, metric=T)) 
  

## sneakily just show where Irvine beach is:
irv <- SpatialPointsDataFrame(coords =cbind(55.603766, -4.710897), data=data.frame(location="Irvine Beach"),
                              proj4string = CRS("+init=epsg:4326"))
carm <-SpatialPointsDataFrame(coords =cbind(51.857543, -4.293021), data=data.frame(location="Carmarthen"),
                              proj4string = CRS("+init=epsg:4326") )
pwant <- carm

# 55.61085966220779, -4.658786430881607
leaflet() %>%
  setView(lng = pwant@coords[2], lat = pwant@coords[1], zoom = 09) %>%
  #addMapPane(name = "maplabels", zIndex = 400) %>% # higher zIndex rendered on top, archaic from labelsonly layer
  addProviderTiles(provwant,options = providerTileOptions(apikey = apithunder)) %>% #"") %>%
  addCircles(data=pwant, lat=pwant@coords[1],lng=pwant@coords[2], fillColor="#dd1c77", color="#dd1c77", weight=19,
              fillOpacity = 0.5, opacity=0.7) %>%
  addScaleBar(position = c("bottomright"), options = scaleBarOptions(imperial=F, metric=T)) 
  

## ==========================================================================
## get place names for those places I want to add to overview plot
## =========================================================================
## place labels are going on top of the survey site labels. take them out and replace select ones
if(!file.exists("places-overview.rds")) {
  places <- c("Bangor, Wales","Cardiff, Wales","Swansea, Wales","Aberystwyth, Wales","Chester, Wales","Wrexham, Wales")
  places <- as.data.frame(geocode(places, output="latlona"))
  places$name <- c("Bangor","Cardiff","Swansea","Aberystwyth","Chester","Wrexham")
  places$cy <- c("Bangor","Caerdydd","Abertawe","Aberystwyth","Caer","Wrecsam")
  
  saveRDS(places, "places-overview.rds")
} 




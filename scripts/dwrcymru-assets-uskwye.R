## look at sewage data released  by dwr cymru

## load libraries required
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
#library(leafgl) #https://github.com/r-spatial/leafgl , might be necessary for lots of data...
library(sp)
library(sf)
library(raster)

library(RColorBrewer)
library(ggplot2)

## read in data
sac <- rgdal::readOGR("../dat-orig/lle/NRW_WFD_MGT_CATCHMENTS_C2", layer="NRW_WFD_MGT_CATCHMENTS_C2Polygon",
                      stringsAsFactors = F)
sac <- spChFIDs(sac, as.character(1:length(sac))) # IMPORTANT!! for indexing need to start at 1 not 0

asscoords <- read.csv("~/static/activism/rivers/assetdatawyeusk-locations.csv", stringsAsFactors = F)
assmeta <- read.csv("~/static/activism/rivers/assetdatawyeusk.csv", stringsAsFactors = F)
assflows <- read.csv("~/static/activism/rivers/assetdatawyeusk-flow.csv", stringsAsFactors = F)

## function
listN <- function(...){ # function that automagically creates named list using objects provided
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}


## take out the clips we want
uskper <- sac[sac$ManCatName=="Usk",]
wyeper <- sac[sac$ManCatName == "Wye",]

## merge wye into one (had it been wales and england)
#wyeper <- gUnaryUnion(wyeper)

## make the asscoords a spatial df and change to leaflet
asscoords <- SpatialPointsDataFrame(coords = SpatialPoints(cbind(asscoords$X, asscoords$Y),
                                                           proj4string = CRS("+init=epsg:27700")), 
                                    data = asscoords)

crslist <- listN(uskper, wyeper, asscoords)
crslist <- lapply(crslist, function(x) {x <- spTransform(x, CRS("+init=epsg:4326"))})

list2env(crslist, envir = globalenv())
rm(crslist)

## identify rownumbers  of assets for usk and wye and combine all data
wyewant <- which(gWithin(asscoords,wyeper, byid = T) == T)
uskwant <- which(gWithin(asscoords,uskper, byid = T) == T)

wyewant <- which(lengths(st_within(asscoords,wyeper))>0)
uskwant <- which(lengths(st_within(asscoords,uskper))>0)

wyedat <- asscoords[wyewant,]
uskdat <- asscoords[uskwant,]

wyedat <- sp::merge(wyedat, assmeta[,c("Asset.ID","Sample.Frequency","Notes")])
uskdat <- sp::merge(uskdat, assmeta[,c("Asset.ID","Sample.Frequency","Notes")])

## tidy the permit types somewhat
table(assmeta$Sample.Frequency, assmeta$Notes, useNA = 'a')

# remove those that are 'not sites' etc.
datlist <- listN(wyedat, uskdat)

datlist <- lapply(datlist, function(x){x <- x[-which(x$Notes %in% c("Not Sampled - No longer operational",
                                                         "Not a site")),]; return(x)})

datlist <- lapply(datlist, function(x){x$Permit <- ifelse(x$Notes=="Not Sampled - No numeric limits in permit", 
                                                          "Unlimited","Limited"); 
return(x)})

list2env(datlist, envir = globalenv())
#rm(crslist)

## plot the overall picture
ggplot(wyedat@data, aes(x=Permit, fill=SEWER_FUNC)) +
  theme_bw() +
  scale_fill_manual("Sewer type", values=c("#018571","#a6611a","#f5f5f5")) +
  geom_bar(colour="black")+ ylab("Count")+
  ggtitle("Wye assets (N = 155)") +
  scale_y_continuous(breaks=seq(0,85, 5))

ggplot(uskdat@data, aes(x=Permit, fill=SEWER_FUNC)) +
  theme_bw() +
  scale_fill_manual("Sewer type", values=c("#018571","#a6611a","#f5f5f5")) +
  geom_bar(colour="black") + ylab("Count") +
  ggtitle("Usk assets (N = 57)")


## spatial
wyedat$River <- "Wye"
uskdat$River <- "Usk"
alldat <- rbind(wyedat, uskdat)

#pid <- sapply(slot(wyeper, "polygons"), function(x) slot(x, "ID"))
wyeper <- SpatialPolygonsDataFrame(wyeper, data=data.frame(River = "Wye", row.names = 19))
uskper <- SpatialPolygonsDataFrame(uskper, data=data.frame(River = "Usk", row.names = 17))
allper <- rbind(wyeper, uskper)

alldat$Permit <- factor(alldat$Permit , levels=c("Limited","Unlimited"))
perpal <- colorFactor(c("#b2182b","#2166ac"), levels(alldat$Permit ))

alldat <- sp::merge(alldat, assflows[,c("Asset.Number","TDV..m3.day.")], by.x='Asset.ID',by.y="Asset.Number")


leaflet() %>% 
  addTiles() %>%
  setView(lng = -4.129263, lat =  52.927390 , zoom = 10) %>%
  addProviderTiles(providers$OpenTopoMap, options=providerTileOptions(minZoom=11)) %>% 
  addProviderTiles("CartoDB.Positron", options=providerTileOptions(maxZoom=10, apikey=apithunder)) %>% # , options = providerTileOptions(opacity = 0)
  addCircleMarkers(data=alldat, fill=~perpal(Permit), color=~perpal(Permit), weight=2,radius = ~ sqrt(TDV..m3.day.),
              fillOpacity = .7, opacity=.9, label=alldat$River,
              group='Assets') %>%
  addPolygons(data=allper, fill=F, color='black', weight = 3,  smoothFactor = 3,
              highlight = highlightOptions(bringToFront = T, stroke=T, color="grey60", fill=F),
              group = 'WFD catchments',
              label=allper$River) %>%
  #addPolylines(data=wfd, color='#762a83', weight = 3, opacity = 0.5, label = wfd$Name, smoothFactor = 3,
   #            highlight = highlightOptions(bringToFront = T, stroke=T, color="grey60", fill=F),
    #           group = 'WFD rivers') %>%
  #addMarkers(data=mills, popup=mills$url, icon=~leafIcons)  %>%
  addLayersControl(
    #baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Assets",'WFD rivers',"SAC WFD sections"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Protected sites", 'WFD rivers',"SAC WFD sections")) %>%
  addLegend("bottomleft", pal = perpal, values=alldat$Permit, # https://github.com/rstudio/leaflet/issues/485
            title = "Permit",group='Assets',
            #labFormat = labelFormat(prefix = "$"),
            opacity = 0.8)  #%>%
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

ggplot(alldat@data, aes(y=TDV..m3.day., x=Permit)) +
  geom_jitter(width=0.25, height=0) +
  geom_violin(fill="transparent", scale="count") +
  facet_wrap(~River)
  

## global script that prepares everything for the shiny-app;
##    gets run each time the app is loaded

## load libraries required
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
#library(leafgl) #https://github.com/r-spatial/leafgl , might be necessary for lots of data...
library(sp)
library(sf)
library(dplyr)

library(RColorBrewer)

## read in my private keys to access some web services
source("./shiny-ignore/keys.R") 

## read in data
prot <- readRDS("./shiny-ignore/Data/protsites-spatial.rds")
wfd <- readRDS("./shiny-ignore/Data/wfdwaters-spatial.rds")
sac <- readRDS("./shiny-ignore/Data/sac-P-spatial.rds")

sacpdat <- readRDS("./shiny-ignore/Data/sac-P.rds")
wfdat <- readRDS("./shiny-ignore/Data/wfd-statuses-cycle3.rds")

perms <- readRDS("./shiny-ignore/Data/consentedoutflows-spatial.rds")
wye <- readRDS("./shiny-ignore/Data/assetswye-spatial.rds")
usk <- readRDS("./shiny-ignore/Data/assetsusk-spatial.rds")

## ===================================================================
## do a bit of tinkering
## ========================================================================
# add p exceedance to sac - but beware the non-unique ID for a mystery wb GB111067057080
#   so need to rough handle that now

## NOTE 'Average annual and growing-season means (March-Sept) were calculated for each
##    sample point and the highest value used for assessment against targets.''
## merge sac p info 
sacout <- sac[sac$ID=="GB111067057080",]
sacpout <- sacpdat[sacpdat$waterbody_=="GB111067057080",]

sac <- sac[sac$ID!="GB111067057080",]
sacpdat <- sacpdat[sacpdat$waterbody_!="GB111067057080",]

sac <- sac %>% left_join(sacpdat[,c("waterbody_","p_standard","exceedence")],
                         by=join_by(ID==waterbody_))

## put the dup id back
sacout$p_standard <- sacpout$p_standard
sacout$exceedence <- sacpout$exceedence 
sac <- rbind(sac, sacout)

## NOTE! If I make a radius proportinal to something, and there are NA, leaflet defaults to 
##    not drawing the feature. So I have to force a value to discharge volume
usk$radiuz <- log(usk$TDV..m3.day.+1)
wye$radiuz <- log(wye$TDV..m3.day.+1)
# for convenience, NA gets the smallest value
wye$radiuz[which(is.na(wye$radiuz))] <- min(wye$radiuz, na.rm = T)
usk$radiuz[which(is.na(usk$radiuz))] <- min(usk$radiuz, na.rm = T)

## =================================================================
## create colour palettes
## ===================================================================

## protected sites
protpal <- colorFactor('BrBG', levels(prot$Designation))

## statuses, and declare factor levels
wfd$Status <- factor(wfd$Status, levels = c("Bad","Poor","Moderate","Good"))
wfdpal <- colorFactor(c('#b2182b','#ef8a62','#fddbc7','#2166ac'),
                      levels = c("Bad","Poor","Moderate","Good")) # NOTE! levels(x) is buggy, doesn't work

sac$Status <- factor(sac$Status, levels=c("Fail","Not Assessed","Comply"))
sacbreaks <- c(-20, -10, 0, 10, 20, 40, 80, 310) # create bins that suit based on values found
sac$statusbinned <- cut(sac$exceedence, breaks = sacbreaks, include.lowest = TRUE)
sac$statusbinned <- addNA(sac$statusbinned)

sacpal <- colorFactor(c("#053061","#4393c3", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"), 
                      levels = c("[-20,-10]","(-10,0]","(0,10]","(10,20]","(20,40]","(40,80]","(80,310]"), 
                      na.color = "black")
# tried to debug this a million years with and without NA as a factor level, and for unexplained
#   reasons it just forces the last colour in colorfactor to the first level. Who knows I give up
#   and yielded to the bug

## type of outflow
sewpal <- colorFactor(c('#a6611a','#a6611a','#80cdc1','#a6611a','#a6611a','#dfc27d','#018571','#018571','#80cdc1','#f5f5f5'), 
                   domain = c("Storm overflow", "Pumping station + Storm overflow",
                              "Pumping station", "Emergency discharge + Pumping station",
                              "Emergency discharge", "Final effluent", "Site drainage",
                              "Surface water", "Process effluent", "Unspecified"),
                   ordered = T) # domain wasn't factor so had to coerce it into knowing this is the order
#   don't really want to order these as these aren't hard boundaries necessarily with severity of pollution

## the numeric vs not limits
perpal <- colorFactor(c("#b2182b","#2166ac"), levels=c("Unlimited","Limited"))

                      
## script that prepares everything for the global script;
##    needs to run each time data layers have changed
## NOTE: run gc() and ggplot2::set_last_plot(NULL) to try to release space...
##     but there still seems to be memory leakage, terra seems better and more flexible

## load libraries
#library(raster)
library(sf)
library(sp)
library(terra)
library(dplyr) # sf is fing tidyverse

## =========================================
## useful functions
## =========================================
nlist <- function(...) { # retain names of objects supplied to a list
  nms <- sapply(as.list(substitute(list(...))), deparse)[-1]
  setNames(list(...), nms)
}

## =========================================
## read in data
## =========================================
## not reading in SPAs; they are designated for birds and are also sssis
## NOTE: ramsar could be more interesting but they are also sssis so leaving for now

sac <- st_read('../dat-orig/lle/NRW_SAC/NRW_SACPolygon.shp') # also sssis; no ISIS ID overlap with NNR
sssi <- st_read('../dat-orig/lle/NRW_SSSI/NRW_SSSI.gpkg') # no ID other than SSSI-specific
#   complains of invalid rings. be aware
nnr <- st_read('../dat-orig/lle/NRW_NNR/NRW_NNRPolygon.shp') # also sssis; no ISIS ID overlap with SAC
lnr <- st_read('../dat-orig/lle/NRW_LNR/NRW_LNRPolygon.shp') # complains of invalid rings. be aware

wfd <- st_read('../dat-orig/lle/WFDRiverCycle3/nrw_wfd_river_waterbodies_c3_baseline_classification.gpkg')
# all rivers monitored under wfd;cycle 3, contains status info too
## FIME: should contain info on water direction but can't find any such colname -- check if still this with c3
##    https://datamap.gov.wales/layers/geonode:nrw_wfd_river_waterbodies_c3_baseline_classification

## the P compliance covers all water bodies of SAC rivers following the new stricter
##    P limits. So this also has status data.
## FIXME: columns not explained. table(assessment0, assessment) not the same and neither
##    of those are the same as overall_as...!!! using overall_as for now
sacp <- st_read('../dat-orig/lle/NRW_SAC_PHOSPHORUS_COMPLIANCE_2017_19/NRW_SAC_PHOSPHORUS_COMPLIANCE_2017_19Polygon.shp')
## hmmmmm why everything else duplicated apart from site_numbe for GB111067057080.... makes joining with other
##    data bad later on.... 


## clean complaining spatial data
sssi <- st_make_valid(sssi) # can check with table(st_is_valid(sssi))
lnr <- st_make_valid(lnr)
sac <- st_make_valid(sac)

## =========================================
## Subset data to what we want
## =========================================
## focus sacs on rivers, searching afon here https://sac.jncc.gov.uk/site/wales
rivercodes <- c("UK0030075","UK0030046","UK0012670","UK0013010","UK0030074",
                "UK0030252","UK0013007","UK0012642")
sac <- sac[sac$Site_code %in% rivercodes,]

## sssis.... fens, bogs and others could perhaps be included but in basics now
## do same, look for afon but also nant; this will now include some coed too
##    as well as cors and who knows (ceunant for example is just a locator not a habitat)
want <- c("afon","nant")
matches <- unique(grep(paste(want, collapse="|"), tolower(sssi$sssi_name), value = T))

sssi <- sssi[tolower(sssi$sssi_name) %in% matches,]

## NNRs... note: all National Nature Reserves are also SSSIs but are owned or leased by NRW
##    however, no way to cross-reference. The data aren't linked. e.g. SSSI Afon Irfon, NNR
##    Nant Irfon. No shared codes. For now will have to duplicate
matches <- unique(grep(paste(want, collapse="|"), tolower(nnr$NNR_Name), value = T))

nnr <- nnr[tolower(nnr$NNR_Name) %in% matches,]

## LNRs... multiple potential ownership generlaly local authority managed; could be more
##    of a mixed bag, they're not also SSSIs
matches <- unique(grep(paste(want, collapse="|"), tolower(lnr$LNR_Name), value = T))

lnr <- lnr[tolower(lnr$LNR_Name) %in% matches,]

## =========================================
## change CRS, combine protected sites to one spatial df and separate
##    water quality/status data from the purely spatial information
## =========================================
## declare ID and status column name as a grep would be mayhem
idcols <- c("ISIS_ID","NNR_Code","Site_code","sssi_id","wbid","waterbody_")
statuscols <- c("overallwb","overall_as")

## objectid is not consistent
colnames(sacp)[1] <- "OBJECTID"
colnames(wfd)[1] <- 'OBJECTID'
colnames(sssi)[1] <- 'OBJECTID'

## create the regular data frames from the condition data
wfdat <- st_drop_geometry(wfd)
sacdat <- st_drop_geometry(sacp)

protnames <- c("LNR", "NNR","SAC","SSSI","WFD","SACP")
prot <- nlist(lnr, nnr, sac, sssi, wfd, sacp)
prot <- lapply(prot, function(x) st_transform(x, crs = 4326))
prot <- lapply(prot, as_Spatial) # died trying to create the below for sf, even applying dplyr failed. fuck tidyverse

prot <- lapply(prot, function(x) {x$Name <- x@data[,grep("name", tolower(colnames(x@data)))[1]]; return(x)})
prot <- Map(function(x,y) {x$ID <- x@data[,y]; return(x)}, prot, idcols)
prot <- Map(function(x,y) {x$Designation <- y; return(x)}, prot, protnames)
prot <- lapply(prot, function(x) {ifelse(length(grep(paste(statuscols, collapse="|"), colnames(x@data)))>0,
                                         x$Status <- x@data[,which(names(x@data) %in% statuscols)],
                                         x$Status <- NA);return(x)})
prot <- lapply(prot, function(x) {x@data <- x@data[,c("OBJECTID","Name","Designation","ID","Status")]; return(x)})
prot <- lapply(prot, function(x) {x <- as(x, "sf")}) # return to sf

## return them to the workspace and relieve prot
list2env(prot, globalenv())
rm(prot)

## now, since almost everything is also sssi and there aren't keys provided to link these up across
##    designations, and the data sets are huge, I'll remove duplication by clipping out all SSSIs
##    covered by my other subsetting, leaving only the SSSIs that are only SSSIs and nothing else
megaprot <- rbind(nnr, sac)  # Combine into one sf object
megaprot <- terra::aggregate(vect(megaprot), cores=2) # trying to remove attributes and create one thing
## NOTE also functions terra::plot which creates a fairly quick plot. this didn't blow up R and memory
##    has calmed down unlike with sf
sssi <- vect(sssi)
sssileft <- erase(sssi, megaprot) # lo and behold, quick
sssileft <- sf::st_as_sf(sssileft)

## put polygons together - note that wfd is spatiallines so need to leave alone
prot <- rbind(lnr, nnr, sac, sssileft )

## save objects
saveRDS(prot, "./shiny-ignore/Data/protsites-spatial.rds")
saveRDS(wfd, "./shiny-ignore/Data/wfdwaters-spatial.rds")
saveRDS(sacp, "./shiny-ignore/Data/sac-P-spatial.rds")

saveRDS(sacdat, "./shiny-ignore/Data/sac-P.rds")
saveRDS(wfdat, "./shiny-ignore/Data/wfd-statuses-cycle3.rds")

## script that prepares everything for the global script;
##    needs to run each time data layers have changed

## load libraries
library(rgdal)
library(rgeos)
library(raster)
library(sf)

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

sac <- readOGR('../dat-orig/lle/NRW_SAC', layer='NRW_SACPolygon')
sssi <- readOGR('../dat-orig/lle/NRW_SSSI', layer='NRW_SSSIPolygon')
nnr <- readOGR('../dat-orig/lle/NRW_NNR', layer='NRW_NNRPolygon')
lnr <- readOGR('../dat-orig/lle/NRW_LNR', layer='NRW_LNRPolygon')

wfd <- readOGR('../dat-orig/lle/NRW_WFD_RIVERS_C2', layer='NRW_WFD_RIVERS_C2Line')
# all rivers monitored under wfd;cycle 2, contains status info too
## NOTE: should contain info on water direction but can't find any such colname
##    https://datamap.gov.wales/layers/inspire-nrw:NRW_WFD_RIVERS_C2

## the P compliance covers all water bodies of SAC rivers following the new stricter
##    P limits. So this also has status data.
## FIXME: columns not explained. table(assessment0, assessment) not the same and neither
##    of those are the same as overall_as...!!! using overall_as for now
sacp <- readOGR('../dat-orig/lle/NRW_SAC_PHOSPHORUS_COMPLIANCE_2017_19', 
                layer='NRW_SAC_PHOSPHORUS_COMPLIANCE_2017_19Polygon')

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
matches <- unique(grep(paste(want, collapse="|"), tolower(sssi$SSSI_NAME), value = T))

sssi <- sssi[tolower(sssi$SSSI_NAME) %in% matches,]

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
idcols <- c("ISIS_ID","NNR_Code","Site_code","SSSI_ID","WBID","waterbody_")
statuscols <- c("overall_as","OverallSta")

## objectid is not consistent
colnames(sacp@data)[1] <- "OBJECTID"

## create the regular data frames from the condition data
wfdat <- as.data.frame(wfd@data)
sacdat <- as.data.frame(sacp@data)

prot <- nlist(lnr, nnr, sac, sssi, wfd, sacp)
#prot <- nlist(lnr, nnr, sac)
prot <- lapply(prot, spTransform, CRS("+init=epsg:4326"))
prot <- lapply(prot, function(x) {x$Name <- x@data[,grep("name", tolower(colnames(x@data)))[1]]; return(x)})
prot <- Map(function(x,y) {x$ID <- x@data[,y]; return(x)}, prot, idcols)
prot <- lapply(prot, function(x) {ifelse(length(grep(paste(statuscols, collapse="|"), colnames(x@data)))>0,
                                         x$Status <- x@data[,which(names(x@data) %in% statuscols)],
                                         x$Status <- NA);return(x)})
prot <- lapply(prot, function(x) {x@data <- x@data[,c("OBJECTID","Name","ID","Status")]; return(x)})

## return them to the workspace
list2env(prot, globalenv())

## put polygons together - note that wfd is spatiallines so need to leave alone
prot <- rbind(lnr, nnr, sac, sssi, sacp, makeUniqueIDs=T )

## save objects
saveRDS(prot, "./shiny-ignore/Data/protsites-spatial.rds")
saveRDS(sacdat, "./shiny-ignore/Data/sac-P.rds")
saveRDS(wfdat, "./shiny-ignore/Data/wfd-statuses-cycle2.rds")

## lle data on consented discharges, which includes non numeric permits (see EIR) 2024
##    storm overflows from rivers trust ## FIXME not in here yet
##    and 'all assets' from dwr cymru possibly 2022

## libraries
library(reshape2)
library(plyr)
library(dplyr)
library(sf)
library(sp)
library(sgo) # transforms BNG to other epsg
library(leaflet)

## read files
perms <- read.csv("../dat-orig/lle/consented_discharges/nrw_water_quality_permits-2024.csv",
                    stringsAsFactors = F)
# metadata which is pretty unhelpful is here https://datamap.gov.wales/layers/geonode:nrw_water_quality_permits

asscoords <- read.csv("../dat-orig/eir/dwrcymru/assetdatawyeusk-locations.csv", stringsAsFactors = F)
assmeta <- read.csv("../dat-orig/eir/dwrcymru/assetdatawyeusk.csv", stringsAsFactors = F)
assflows <- read.csv("../dat-orig/eir/dwrcymru/assetdatawyeusk-flow.csv", stringsAsFactors = F)
sac <- st_read("../dat-orig/lle/NRW_WFD_MGT_CATCHMENTS_C2/NRW_WFD_MGT_CATCHMENTS_C2Polygon.shp")
# need the above to be able to tell wye and usk apart in the data set



## function
listN <- function(...){ # function that automagically creates named list using objects provided
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

## ==============================================================
## prep consented discharges
## ===============================================================
## look for non numerics
unique(perms$determinand) # Descriptive Consent : Pass/Fail 1/0; there is also "".....
#   and Site Inspection : Pass/Fail (1/0), "Weir Setting"  

## repeated rows for permit ID, let's check if determinands are indeed long format
## FIXME what are then the limit 1,2,3?
perms[perms$permit_number=="CB3094HL",]
## e.g. here there are three determinands, limits 2 and 3 NA just limit 1 info.

## summarise how many determinands per permit
permsum <- ddply(perms, .(permit_number), summarise, nconds = length(determinand_code))

## what might empty strings be?
perms[which(perms$determinand==""),] # defo no numeric limits, NA abound incl for determinand_code
perms[which(perms$determinand=="Descriptive Consent : Pass/Fail 1/0"),] # 7444; looks very non numeric
perms[which(perms$determinand=="Site Inspection : Pass/Fail (1/0)"),] # 4883; looks very non numeric
perms[which(perms$determinand=="Weir Setting"),] # 8174; this has variable limits. might treat as numeric
#   but FIXME what is 9999, is this again effectively non numeric?
## NOTE have sent NRW query on whether I am correctly filtering the data

## now let's look at what there is in terms of who they're regulating
table(perms[!duplicated(perms$permit_number),"operator_party_type"]) # 4617/6573

## for now let's focus on Dwr Cymru
dat <- perms[which(perms$operator_party_type=="Company"),]
## still a lot of companies! 
## looks like DWR CYMRU CYFYNGEDIG is the only id referring to the company
dat <- dat[which(dat$operator=="DWR CYMRU CYFYNGEDIG"),] # leaves 2949 permits

## let's assume for now that non numeric is all 9999 entries on limit_1 as well as our determinands of interest
dat <- dat[which(dat$limit_1==9999 | dat$determinand_code %in% c(7444,4883) | is.na(dat$determinand_code)),]

## do any of them also have numeric permits? (assuming we are interested only in those with no numeric limits at all)
## ergo, remove these objectids from perms and check for matching permit numbers, then remove those
perms <- perms[-which(perms$objectid %in% dat$objectid),] # checked that this worked
numerics <- unique(dat$permit_number[which(dat$permit_number %in% perms$permit_number)]) # 1415!!
dat <- dat[-which(dat$permit_number %in% numerics),] # 1002 permits left.

## FIXME - why do dwr cymru outflows have release type = not water company? surely that is a mistake?
## let's check if that precipitates across the permits that have many determinands
## Anyway, let's also make the column more user friendly
reltype <- data.frame("release_type"=unique(dat$release_type), "release"=NA)
reltype$release <- c("Storm overflow","Pumping station","Emergency discharge",
                     "Final effluent", "Storm overflow","Pumping station",
                     "Storm overflow","Storm overflow", "Site drainage",
                     "Surface water","Process effluent","Unspecified")
dat <- merge(dat, reltype, sort=F)

typecheck <- ddply(dat, .(permit_number), dplyr::summarise, nconds = n(), ntype=length(levels(as.factor(release_type))))
typecheck[which(typecheck$ntype>1),] 
mults <- typecheck$permit_number[which(typecheck$ntype>1)]
releases <- unique(dat[dat$permit_number %in% mults,c("permit_number","release_type", "release")])
## they are all pumping station + emergency/storm overflows. (SC, SB, SD, DA), none of them not company

## paste the doubles together
releases <- releases[order(releases$permit_number, releases$release),]
releasesum <- ddply(releases, .(permit_number), summarise, release=paste0(release, collapse = " + "))
## some ultimately counted as double storm overflow
releasesum <- releasesum[-which(releasesum$release=="Storm overflow + Storm overflow"),]

## Join A and B
dat <- dat %>%
  left_join(releasesum, by = "permit_number", suffix = c(".A", ".B"))  # Adjust 'id' if your ID column has a different name
## Replace values from B into A where applicable
dat <- dat %>%
  mutate(release.A = coalesce(release.B, release.A)) %>%
  # Repeat the above line for other columns as needed
  select(-contains(".B")) %>%  # Remove B columns after replacement
  # Rename back to original column names if necessary
  dplyr::rename(release = release.A)  

## summarise for dat now which places are left
datsum <- ddply(dat, .(permit_number), summarise, ndets = length(determinand_code), receiver = receiving_waters[1],
                release=release[1])
# table(datsum$release)

## remove all annoying cols
datwant <- c("objectid","permit_number","release","determinand_code", "determinand", 
             "limit_1" ,"outlet_ngr","receiving_waters")
dat <- dat[,c(datwant)]

## make into spatial
ukgrids <- dat$outlet_ngr
## check that they are valid
checks <- sgo_ngr_bng(ukgrids, check.only = T)
## make into bng
ukgrids <- sgo_ngr_bng(ukgrids)
## BNG to WGS84
coords <- sgo_bng_lonlat(ukgrids, to=4326)
## create spatial object
datspat <- SpatialPointsDataFrame(coords =cbind(coords$x, coords$y), data = dat, 
                                  proj4string = CRS("+init=epsg:4326"))
datspat <- st_as_sf(datspat)

## ==================================================================
## prep EIR dwr cymru data
## =====================================================================
## make the asscoords a spatial df and change to leaflet
asscoords <- SpatialPointsDataFrame(coords = SpatialPoints(cbind(asscoords$X, asscoords$Y),
                                                           proj4string = CRS("+init=epsg:27700")), 
                                    data = asscoords)
## now that sf wins, make into sf for list structure
asscoords <-  as(asscoords, "sf") # return to sf

## take out what we need for the sace
uskper <- sac[sac$ManCatName=="Usk",]
wyeper <- sac[sac$ManCatName == "Wye",]

crslist <- listN(uskper, wyeper, asscoords)
crslist <- lapply(crslist, function(x) {x <- st_transform(x, crs = 4326)})

list2env(crslist, envir = globalenv())
rm(crslist)

## identify rownumbers  of assets for usk and wye and combine all data
wyewant <- which(lengths(st_within(asscoords,wyeper))>0)
uskwant <- which(lengths(st_within(asscoords,uskper))>0)

wyedat <- asscoords[wyewant,]
uskdat <- asscoords[uskwant,]

## NOTE!!! It seems sp::merge is fragile with sf objects. all.x is not so, and it deletes missings insted
##    of filling. Fucking bastard sf.
wyedat <- sp::merge(wyedat, assmeta[,c("Asset.ID","Sample.Frequency","Notes")])
uskdat <- sp::merge(uskdat, assmeta[,c("Asset.ID","Sample.Frequency","Notes")])

wyedat <- left_join(wyedat, assflows[,c("Asset.Number","TDV..m3.day.")], by=join_by(Asset.ID == Asset.Number))
uskdat <- left_join(uskdat, assflows[,c("Asset.Number","TDV..m3.day.")], by=join_by(Asset.ID == Asset.Number))

# remove those that are 'not sites' etc.
datlist <- listN(wyedat, uskdat)

datlist <- lapply(datlist, function(x){x <- x[-which(x$Notes %in% c("Not Sampled - No longer operational",
                                                                    "Not a site")),]; return(x)})

datlist <- lapply(datlist, function(x){x$Permit <- ifelse(x$Notes=="Not Sampled - No numeric limits in permit", 
                                                          "Unlimited","Limited"); 
return(x)})

list2env(datlist, envir = globalenv())


## =============================================================
## save dats
## ===============================================================
saveRDS(datspat, "./shiny-ignore/Data/consentedoutflows-spatial.rds")
saveRDS(wyedat, "./shiny-ignore/Data/assetswye-spatial.rds")
saveRDS(uskdat, "./shiny-ignore/Data/assetsusk-spatial.rds")


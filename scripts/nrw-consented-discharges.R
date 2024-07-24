## lle data on consented discharges... so we have f'ing consented discarges from lle,
##    storm overflows from rivers trust and 'all assets' from dwr cymru... will be fun to see
##    how the data relate trololol (also files for 2016 vs 2022!? yay)
## UPDATE 2024 Feb a further new version of the data has arrived.

## libraries
library(reshape2)
library(plyr)
library(dplyr)
library(sf)
library(sp)
library(sgo) # transforms BNG to other epsg
library(leaflet)

consa <- read.csv("../dat-orig/lle/consented_discharges/AFA184_NRW_DS116329 WIMS Data WITH Conditions up to 24_06_2016-north.csv",
                  stringsAsFactors = F)
consb <- read.csv("../dat-orig/lle/consented_discharges/AFA184_NRW_DS116329 WIMS Data WITH Conditions up to 24_06_2016-se.csv",
                  stringsAsFactors = F)
consc <- read.csv("../dat-orig/lle/consented_discharges/AFA184_NRW_DS116329 WIMS Data WITH Conditions up to 24_06_2016-sw.csv",
                  stringsAsFactors = F)

consold <- rbind(consa, consb, consc)

consnew <- read.csv("../dat-orig/lle/consented_discharges/NRW_DS116329 Consented Discharges to Controlled Waters APR22.csv",
                    stringsAsFactors = F)
conscur <- read.csv("../dat-orig/lle/consented_discharges/nrw_water_quality_permits-2024.csv",
                    stringsAsFactors = F)
# metadata which is pretty unhelpful is here https://datamap.gov.wales/layers/geonode:nrw_water_quality_permits

table(unique(consold$Consent.Number) %in% unique(consnew$Permit.ref))#wtf
table(consold$Date.Revoked) #wtf why over 1000 revoked 31s oct 96???
table(consold$Consent.Status, consold$Date.Revoked=='') # yay more vagye patterns fuck

## test if revokation is the diff
notrev <- consold$Consent.Number[consold$Date.Revoked=='']
table(unique(notrev) %in% consnew$Permit.ref) # jesus fucking christ pattern but not consistent

## let's look at some fucking individual matches and see what' s there
head(unique(consold$Consent.Number)[(unique(consold$Consent.Number) %in% unique(consnew$Permit.ref))])

consold[consold$Consent.Number=="CG0076401",] ##oh right!! great!! revoked and reissued (presumably 
# under differnt conditions) permits still have the same code!! fucking twats

table(consnew$Revocation.date) # none revoked here so presumably a list containing only active permits

## fuck off the consents from old that have been revoked
consold <- consold[-which(consold$Date.Revoked!=''),]

consold[consold$Consent.Number=="CG0076401",] 

table(unique(consnew$Permit.ref) %in% unique(notrev)) # jesus fucking christ pattern but not consistent

## ok so trying to pull together some traceable column names that might make sense
unique(consold$Determinand.Code.Description)
## FIXME might descriptive consent refer to non-numeric limits?

## =========================================================================================
## focusing on 2024 data for now
## =========================================================================================
## look for non numerics
unique(conscur$determinand) # still Descriptive Consent : Pass/Fail 1/0; there is also "".....
#   and Site Inspection : Pass/Fail (1/0), "Weir Setting"  

## repeated rows for permit ID, let's check if determinands are indeed long format
## FIXME what are then the limit 1,2,3?
conscur[conscur$permit_number=="CB3094HL",]
## e.g. here there are three determinands, limits 2 and 3 NA just limit 1 info.

## summarise how many determinands per permit
perms <- ddply(conscur, .(permit_number), summarise, nconds = length(determinand_code))

## what might empty strings be?
conscur[which(conscur$determinand==""),] # defo no numeric limits, NA abound incl for determinand_code
conscur[which(conscur$determinand=="Descriptive Consent : Pass/Fail 1/0"),] # 7444; looks very non numeric
conscur[which(conscur$determinand=="Site Inspection : Pass/Fail (1/0)"),] # 4883; looks very non numeric
conscur[which(conscur$determinand=="Weir Setting"),] # 8174; this has variable limits. might treat as numeric
#   but FIXME what is 9999, is this again effectively non numeric?

## now let's look at what there is in terms of who they're regulating
table(conscur[!duplicated(conscur$permit_number),"operator_party_type"]) # 4617/6573

## for now let's focus on Dwr Cymru
dat <- conscur[which(conscur$operator_party_type=="Company"),]
## still a lot of companies! 
## looks like DWR CYMRU CYFYNGEDIG is the only id referring to the company
dat <- dat[which(dat$operator=="DWR CYMRU CYFYNGEDIG"),] # leaves 2949 permits

## let's assume for now that non numeric is all 9999 entries on limit_1 as well as our determinands of interest
dat <- dat[which(dat$limit_1==9999 | dat$determinand_code %in% c(7444,4883) | is.na(dat$determinand_code)),]

## do any of them also have numeric permits? (assuming we are interested only in those with no numeric limits at all)
## ergo, remove these objectids from conscur and check for matching permit numbers, then remove those
conscur <- conscur[-which(conscur$objectid %in% dat$objectid),] # checked that this worked
numerics <- unique(dat$permit_number[which(dat$permit_number %in% conscur$permit_number)]) # 1415!!
dat <- dat[-which(dat$permit_number %in% numerics),] # 1002 permits left.

## FIXME - why do dwr cymru outflows have release type not water company? surely that is a mistake?
## let's check if that precipitates across the permits that have many determinands
## Anyway, let's make the column more user friendly
reltype <- data.frame("release_type"=unique(dat$release_type), release=NA)
reltype$release <- c("Storm overflow","Pumping station","Emergency discharge",
                     "Final effluent", "Storm overflow","Pumping station",
                     "Storm overflow","Storm overflow", "Site drainage",
                     "Surface water","Process effluent","Unspecified")
dat <- merge(dat, reltype, sort=F)

typecheck <- ddply(dat, .(permit_number), summarise, nconds = n(), ntype=length(levels(as.factor(release_type))))
typecheck[which(typecheck$ntype>1),] #trolololol
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
  rename(release = release.A)  

## summarise for dat now which places are left
datsum <- ddply(dat, .(permit_number), summarise, ndets = length(determinand_code), receiver = receiving_waters[1],
                release=release[1])

table(datsum$release)

## ========================================================================================
## map what we've found out
## =======================================================================================
ukgrids <- dat$outlet_ngr

## check that they are valid
checks <- sgo_ngr_bng(ukgrids, check.only = T)
## make into bng
ukgrids <- sgo_ngr_bng(ukgrids)
## BNG to WGS84
coords <- sgo_bng_lonlat(ukgrids, to=4326)

## create spatialpointsdataframe
datspat <- SpatialPointsDataFrame(coords =cbind(coords$x, coords$y), data = dat, 
                                  proj4string = CRS("+init=epsg:4326"))

pal <- colorFactor(c('#a6611a','#a6611a','#80cdc1','#a6611a','#a6611a','#dfc27d','#018571','#018571','#80cdc1','#f5f5f5'), 
                   domain = c("Storm overflow", "Pumping station + Storm overflow",
                              "Pumping station", "Emergency discharge + Pumping station",
                              "Emergency discharge", "Final effluent", "Site drainage",
                              "Surface water", "Process effluent", "Unspecified"),
                   ordered = T) # domain wasn't factor so had to coerce it into knowing this is the order
#['#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571']

leaflet(df) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(type == "ship", 6, 10),
    color = ~pal(type),
    stroke = FALSE, fillOpacity = 0.5
  )

leaflet(datspat) %>%
  addProviderTiles(provider = providers$OpenTopoMap) %>%
  setView(lat=51.82098, lng=-3.01743, zoom=8) %>%
  addCircleMarkers(data=datspat[-which(duplicated(datspat$permit_number)),],
                   fillColor=~pal(release), color="black", weight=1, fillOpacity=0.7, popup = ~release,
                   clusterOptions = markerClusterOptions(maxClusterRadius = 25,
                                                         disableClusteringAtZoom = 11))

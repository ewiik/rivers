## look at failures and targets at the wye

## libraries
library("ggplot2")
library("plyr")
library("dplyr")
library("reshape2")
library("rgeos")
library("rgdal")
library("sf")


## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

## read in files
tar <- read.csv("SACtargets2020.csv", stringsAsFactors = F) # from waterwatch
fail <- read.csv("waterbody-statuses.csv", stringsAsFactors = F) # I think also from waterwatch

prep <- readOGR("SAC-P/ComplianceAssessmentOfWelshRiverSacsAgainstPhosphorusTargets", # from lle
                layer = "NRW_PHOSPHORUS_SENSITIVE_SAC_FRESHWATER_CATCHMENTSPolygon")
## the actual P data don't exist even though they should exist based on the metadata
##    but is actually linked with the P report on that portal...
pdat <- read.csv("sac-phosphorus-assessment-data-annmean.csv", stringsAsFactors = F)
# https://naturalresources.wales/evidence-and-data/research-and-reports/water-reports/compliance-assessment-of-welsh-river-sacs-against-phosphorus-targets/?lang=en

fgdb <- "WFDRiverCycle2/WFDRCWC2.gdb" # from lle

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

fc <- readOGR(dsn=fgdb,layer="WFD_River_Waterbodies_Cycle2")

## codes don't seem to be very sensible
fail <- fail[fail$WB_TYPE_NAME=="River",]

tar <- tar[tar$SAC.Name=="River Wye / Afon Gwy",]
tar[!tar$Water.Body.ID %in% fail$WATERBODY_ID,c(1:10)] # just 1 so will roll with it

tar <- merge(tar, fail[,c("WATERBODY_ID","NBB2015.Overall.Status", "NBB2015.EcoStatus","NBB2015.ChemStatus",
                          "NBB2015.Quantitative", "NBB2015ElemFail", "OverallObj")], 
                          by.x="Water.Body.ID", by.y="WATERBODY_ID", all.y = F, all.x = T, sort=F)

fc <- fc[fc$WBID %in% tar$Water.Body.ID,]
table(fc$P)
table(fc$Ammonia)
table(fc$DO)
table(fc$Invertebrates)
table(fc$ECO_BIO)

tar$PugL <- tar$Phosphorus..standard..mg.L. * 1000

table(tar$NBB2015.ChemStatus, tar$NBB2015.EcoStatus, tar$NBB2015.Overall.Status, dnn = c("chem", "eco","overall"))
# yep assigned logically

tar$PFail <- "No"
tar$PFail[grep("P", tar$NBB2015ElemFail)] <- "Yes"

## subset P report p annual means to gwy
pdat <- pdat[pdat$SACName=="River Wye / Afon Gwy",]

## plot target P
ggplot(tar, aes(x=as.factor(PugL), fill=NBB2015.EcoStatus)) +
  geom_bar() +
  xlab("Phosphorus (ug/L)") + ylab("Number of sites")

ggplot(tar, aes(x=as.factor(PugL), fill=PFail)) +
  papertheme +
  geom_bar() +
  xlab("Phosphorus target (ug/L)") + ylab("Number of sites") +
  scale_fill_manual("Failing on P (2015)?", values=c("#4575b4","#d73027"))

ggplot(pdat, aes(x=as.factor(P_standard_ÂµgL), fill=Overall_Ass)) + #Assessment)) +
  papertheme +
  geom_bar(col="black") +
  xlab("Phosphorus target (ug/L)") + ylab("Number of sites") +
  scale_fill_manual("Failing on P (2021)?", values=c("#4575b4","#d73027","transparent"))

w2021 <-
ggplot(droplevels(pdat[pdat$Overall_Ass!="Not Assessed",]), aes(x=P_standard_µgL, y= Ann_Mean_P_ug_L, fill=Overall_Ass)) +
  papertheme +
  geom_point(shape=21, size=3, alpha=0.7) +
  geom_abline(slope=1, intercept = 0) +
  xlab("Phosphorus target (ug/L)") + ylab("Annual mean (ug/L)") +
  scale_fill_manual("Failing on P (2021)?", values=c("#4575b4","#d73027","transparent"))

ggsave("./R-products/Gwy-2021.jpeg", w2021, width=7, height=4)

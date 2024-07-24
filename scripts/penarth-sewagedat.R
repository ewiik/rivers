## look at specific rivers

dat <- read.csv("~/git/rivers/dat-orig/riverstrust/Event_Duration_Monitoring_-_Storm_Overflows_-_2022_(England_and_Wales).csv",
                stringsAsFactors = F)
unique(dat$waterCompanyName)

dat <- dat[which(dat$waterCompanyName=="Dwr Cymru Welsh Water"),]

ely <- dat[grep("ely", tolower(dat$wfdWaterbodyName)),]
elycheck <- dat[grep("ely", tolower(dat$recievingWaterName)),]

## upon personal inspection, the elies sound legit. zero-rows in check
hours <- sum(ely$totalDurationAllSpillsHrs, na.rm = T)
spills <- sum(ely$countedSpills, na.rm = T)

## manually select the ones by Penarth.
penarths <- c("CSO and EO at Brockhill Rise and Lavernock Road Pumping Station",
              "CSO and EO at Kymin Pumping Station",
              "Penarth Marina SPS")
penarth <- dat[which(dat$siteNameEA %in% penarths),]
penarth[,c("siteNameEA","totalDurationAllSpillsHrs","countedSpills")]

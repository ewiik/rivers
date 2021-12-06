## read in some high-res climate projections from metoffice
## note that the 2.2 downloads have projections from 12 members

## wye data are from newbridge-on-wye using Clews et al relationships between temps
##    the elevation of the location is roughtly 160m. 
## UKCP products give temp 1.5m, above ground apparently 
##    (see https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiW0-vL87rzAhWUnVwKHVnaBJoQFnoECAMQAQ&url=https%3A%2F%2Fsafety.networkrail.co.uk%2Fwp-content%2Fuploads%2F2019%2F09%2FClimate-change-projections-Guidance-Note.pdf&usg=AOvVaw1a3e_q9e8sdtzkq8v6nChV)

## libraries
library(ggplot2)
library(gridExtra)
library(viridis) 
library(reshape2)
library(plyr)
library(dplyr)

## read in file(s)
sitewant <- "wye"

if(sitewant=="usk") { # not currently part of script
  dat <- read.csv("",
                  skip = 12, stringsAsFactors = F, 
                  col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
  
  ldat <- read.csv("",
                   skip = 12, stringsAsFactors = F, 
                   col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
} else {
  bdat <- read.csv("../dat-orig/local-daily-temp-baseline/subset_2021-12-03T13-17-44.csv",
                  skip = 12, stringsAsFactors = F, 
                  col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
  
  mdat <- read.csv("../dat-orig/local-daily-temp-mid/subset_2021-12-03T13-27-05.csv",
                   skip = 12, stringsAsFactors = F, 
                   col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
  
  edat <- read.csv("../dat-orig/local-daily-temp-end/subset_2021-12-03T13-28-29.csv",
                  skip = 12, stringsAsFactors = F, 
                  col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
  
}

## =======================================================
## some useful functions
## ==================================
listN <- function(...){ # function that automagically creates named list using objects provided
  anonList <- list(...)
  names(anonList) <- as.character(substitute(list(...)))[-1]
  anonList
}

## make date into date and create other columns
## NOTE: metoffice work on an artificial year of 360 days with each month having 30 days, so NAs will result for missing dates
##    and imaginary dates.
dodates <- function(df){
  df$Date <- as.POSIXct(df$Date, tz="",format="%Y-%m-%d")
  #ind <- which(is.na(df$Date))
  #diff(df$Date)
  df$Year <- as.numeric(format(df$Date, format="%Y"))
  df$Month <- as.numeric(format(df$Date, format="%m"))
  df$DoY <- as.numeric(format(df$Date, format="%j"))
  return(df)
}

## summarise durations of warm spells
warmdur <- function(df){
  #df <- ddply(df, .(), summarise, Year=paste0(unique(Year), sep=","), Month=paste0(unique(Month), sep=","), Duration=n())
  newdf <- data.frame(Year=ifelse(length(unique(df$Year))>1, mean(c(unique(df$Year))), df$Year[1]), 
                      Month=ifelse(length(unique(df$Month))>1, mean(c(unique(df$Month))), df$Month[1]), 
                      Duration=nrow(df), Start=min(df$DoY), End=max(df$DoY), Mid=mean(df$DoY))
  return(newdf)
}

warmdurlist <- function(alist){
  #df <- ddply(df, .(), summarise, Year=paste0(unique(Year), sep=","), Month=paste0(unique(Month), sep=","), Duration=n())
  newlist <- lapply(alist, function (x) {
  x <- as.data.frame(x)  
  newx <- data.frame(Year=ifelse(length(unique(x$Year))>1, mean(c(unique(x$Year))), x$Year[1]), 
                      Month=ifelse(length(unique(x$Month))>1, mean(c(unique(x$Month))), x$Month[1]), 
                      Duration=nrow(x), Start=min(x$DoY), End=max(x$DoY), Mid=mean(x$DoY))
  return(newx)})
  newlist <- do.call(rbind, newlist)
  return(newlist)
  }

## using head and other water correlation from clews et al 2010  https://doi.org/10.1111/j.1365-2486.2010.02211.x
## Newbridge-on Wye (after Ormerod, 1985) Ormerod SJ (1985) The distribution of aquatic macroinvertebrates in the catchment of the
##    Welsh River Wye in relation to ionic composition. PhD Thesis, University of Wales.
rivtemp <- function(airtemp) {out <- 3.07 + 0.942*airtemp; return(out)} # main channels
headtemp <- function(airtemp) {out <- 3.4 + 0.715*airtemp;return(out)} # headwaters

## set threshold for use in subsetting
sumthresh <- 19.5 # lower thresh of brown trout for no growth
authresh <- 12 # above this abnormalities etc.

autDoY <- 288 # 15th Oct in a non-leap year

## ==================================================================================
## data handling
## =====================================================================================
## create list of data we want to work with
datlist <- listN(bdat, mdat, edat)

## apply dates
datlist <- lapply(datlist, dodates)

## apply to list and return to env
list2env(datlist, envir = globalenv())

## for now will just remove the NA dates from the set
#dat <- dat[-which(is.na(dat$Date)),]

## combine to alldat
alldat <- rbind(bdat, mdat, edat)

## melt so that each projection is longformat
allmelt <- melt(alldat, measure.vars = grep("X", names(alldat)), variable.name = "Projection",
                value.name = "Temperature")

## create river temperature
allmelt$rivtemp <- rivtemp(allmelt$Temperature)
allmelt$headtemp <- headtemp(allmelt$Temperature)

## summarise the difference between days by projection
warmdat <- allmelt[allmelt$rivtemp>sumthresh,]

warmdat <- split(warmdat, warmdat$Projection)
warmdat <- lapply(warmdat, function(x) {
x$diff <- c(0,diff(x$Date));return(x)})

## consider both 1 and 2 for simplicity as consecutive, given the gaps in real dates in the data set
breaks <- lapply(warmdat, function(x) {x <- which(x$diff>2)})
## create a split of a split --> nested list of 12
warmsplit <- Map(function(x,y) {split(x, cumsum(1:nrow(x) %in% y))}, warmdat, breaks)

## check when these are, i.e. summarise
warmdat <- lapply(warmsplit, warmdurlist) 
warmdat <- Map(function(x,y){x$Projection <- y; return(x)}, warmdat, names(warmdat))
warmdat <- do.call(rbind, warmdat)

# rankt the 12 projections to get an idea of best and worst - in terms of future
warmrank <- warmdat[-which(warmdat$Year < 2021),]
warmrank <- ddply(warmrank, .(Projection), summarise, totDur=sum(Duration), meanDur=mean(Duration))
warmrank <- warmrank[order(warmrank$totDur, decreasing = T),]
warmrank$rank <- 1:nrow(warmrank)
want <- warmrank$Projection[warmrank$rank %in% c(1,6,12)]

warmdat$ProjFac <- factor(warmdat$Projection, levels = warmrank$Projection)

## what about the situation for our spawning times? let's start with an october to december frame
## then do all as for warmdat but for headwaters
coldat <- allmelt[which(allmelt$Month > 9 & allmelt$Month <=12 & allmelt$headtemp > authresh),]
coldat <- split(coldat, coldat$Projection)
coldat <- lapply(coldat, function(x) {
  x$diff <- c(0,diff(x$Date));return(x)})
breaks <- lapply(coldat, function(x) {x <- which(x$diff>2)})
colsplit <- Map(function(x,y) {split(x, cumsum(1:nrow(x) %in% y))}, coldat, breaks)
coldat <- lapply(colsplit, warmdurlist) 
coldat <- Map(function(x,y){x$Projection <- y; return(x)}, coldat, names(coldat))
coldat <- do.call(rbind, coldat)

coldrank <- coldat[-which(coldat$Year < 2021),]
coldrank <- ddply(coldrank, .(Projection), summarise, totDur=sum(Duration), meanDur=mean(Duration))
coldrank <- coldrank[order(coldrank$totDur, decreasing = T),]
coldrank$rank <- 1:nrow(coldrank)
coldwant <- coldrank$Projection[coldrank$rank %in% c(1,6,12)]

coldat$ProjFac <- factor(coldat$Projection, levels = coldrank$Projection)

## ===========================================================================================
## plots 
## ==========================================================================================
## plot something from the raw data
today <-
ggplot(dat,aes(y=Year, x=DoY, z=X1649, fill=X1649)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis()

tomorrow <-
ggplot(ldat,aes(y=Year, x=DoY, z=X1649, fill=X1649)) +
  theme_bw() +
  geom_tile() +
  scale_fill_viridis()

grid.arrange(today, tomorrow, ncol=2)


## plot warm day summary (how many days in a year are warmer than threshold)
ggplot(warmsum, aes(y=count, x=Year)) +
  theme_bw() +
  geom_bar(stat="identity")

## plot warm spell summary (duration of days warmer than threshold)
ggplot(warmdat, aes(x=Year, y=Duration, fill=Duration)) +
  theme_bw() +
  geom_bar(stat="identity") +
  scale_fill_viridis() +
  facet_wrap(~Month, nrow=2)

doydat <- data.frame(x=2, y=c(280,260,240,160, 140), label=c('Oct 7',"Sep 17","Aug 28","Jun 9","May 20"))

## plot warm spells for entire year
warmyear <-
ggplot(warmdat[warmdat$ProjFac %in% want,], aes(x=as.factor(Year), y=Mid, col=Duration>7)) +
  theme_bw() +
  geom_linerange(aes(ymin=Start, ymax=End), size=2) +
  scale_color_manual("Duration > 1 week", values=c("#2166ac","#b2182b")) +
  ylab("Day of Year") + xlab("Year (vertical line = break between projection sets)") +
  theme(axis.text.x = element_text(hjust=1, angle=45)) +
  geom_vline(xintercept = c(20.5, 40.5), linetype="dashed") + # this expresses the midpoint from length of years
  ggtitle("River temperatures exceeding brown trout growth limit in main river") +
  scale_y_continuous(breaks=seq(140,280, by=10)) +
  geom_text(data=doydat, inherit.aes = F, aes(x=x, y=y, label=label), size=4) +
  facet_wrap(~ProjFac, ncol=1)

## plot warm spells for spawning period
warmaut <-
ggplot(coldat[coldat$ProjFac %in% coldwant,], aes(x=as.factor(Year), y=Mid, col=Duration>7)) +
  theme_bw() +
  geom_linerange(aes(ymin=Start, ymax=End), size=2) +
  scale_color_manual("Duration > 1 week", values=c("#2166ac","#b2182b")) +
  ylab("Day of Year (October to December)") + xlab("Year (vertical line = break between projection sets)") +
  theme(axis.text.x = element_text(hjust=1, angle=45)) +
  geom_vline(xintercept = c(20.5, 40.5), linetype="dashed") + # this expresses the midpoint from length of years
  ggtitle("River temperatures exceeding healthy egg development in headwaters") +
  scale_y_continuous(breaks=seq(275,375, by=20)) +
  geom_hline(aes(yintercept = autDoY), linetype="dashed") +
  facet_wrap(~ProjFac, ncol=1)

grid.arrange(warmyear, warmaut, ncol=1)

## print out figs
ggsave("")

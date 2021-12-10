## sewage analysis
## daily mm precipitation


odat <- read.csv("subset_2021-10-15T17-42-42.csv",
                skip = 12, stringsAsFactors = F, 
                col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))
newdat <-  read.csv("sewage-wales-lastslice/subset_2021-10-15T17-52-35.csv",
                    skip = 12, stringsAsFactors = F, 
                    col.names = c("Date",'0','1113','1554','1649','1843','1935','2123','2242','2305','2335','2491','2868'))

odat$slice <- "Past" # 1981-2000
newdat$slice <- "Last" # 2061-2080

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

## summarise rain spells
raincum <- function(df){
  #df <- ddply(df, .(), summarise, Year=paste0(unique(Year), sep=","), Month=paste0(unique(Month), sep=","), Duration=n())
  newdf <- data.frame(Year=ifelse(length(unique(df$Year))>1, mean(c(unique(df$Year))), df$Year[1]), 
                      Month=ifelse(length(unique(df$Month))>1, mean(c(unique(df$Month))), df$Month[1]), 
                      Duration=nrow(df), Start=min(df$DoY), End=max(df$DoY), Mid=mean(df$DoY))
  return(newdf)
}

## four-day accumulated rain
raincum <- function(df) {
  df$rainsum <- NA
  for (i in 4:nrow(df)) {
  df$rainsum[i] <- sum(df$X2868[(i-3):i])}
  return(df)}
  
## =================================================================================================
## create dates and rainfall accumulation info
## ====================================================================================================
datlist <- listN(odat, newdat)

datlist <- lapply(datlist, raincum)
datlist <- lapply(datlist, dodates)

list2env(datlist, envir = globalenv())
rm(datlist)

alldat <- rbind(odat, newdat)

alldat$Season <- "Summer"
alldat$Season[alldat$Month > 9 | alldat$Month <5] <- "Not summer"

## ===================================================================================================
## summarise how many days in random projection is exceeding four-day limit
## =================================================================================================
rainsum <- ddply(alldat, .(slice,Year, Season), summarise, Exceed = length(which(rainsum>26.6)))
rainsum <- na.omit(rainsum)

ggplot(rainsum, aes(y=Exceed, x=as.factor(Year))) + # identity still works out the sum of both Seasons
  theme_bw() +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(rainsum, aes(y=Exceed, x=as.factor(Year), fill=Season)) + 
  theme_bw() +
  geom_bar(stat="identity", na.rm=T) +
  scale_fill_manual(values=c("#5ab4ac","#d8b365")) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ylab("Days of exceeding threshold") + xlab("Year") #+
#  facet_grid(~Season)

t.test(rainsum$Exceed[rainsum$slice=="Last"], rainsum$Exceed[rainsum$slice!="Last"]) # not signif
t.test(rainsum$Exceed[rainsum$slice=="Last" & rainsum$Season=="Summer"], rainsum$Exceed[rainsum$slice!="Last" & rainsum$Season=="Summer"]) # signif
t.test(rainsum$Exceed[rainsum$slice=="Last" & rainsum$Season!="Summer"], rainsum$Exceed[rainsum$slice!="Last" & rainsum$Season!="Summer"]) # signif

raindist <- ddply(rainsum, .(slice, Season), summarise, Mean=mean(Exceed, na.rm=T), sd=sd(Exceed, na.rm=T))
raindist$slice <- factor(raindist$slice, levels=c("Past","Last"))

ggplot(raindist, aes(y=Mean, x=interaction(slice, Season), color=Season, fill=Season)) +
  theme_bw() +
  geom_point(shape=21, size=5) +
  geom_linerange(aes(ymax=Mean+sd, ymin=Mean-sd), size=1.5) +
  scale_x_discrete(name="",labels=c("1981-2000","2061-2080","1981-2000","2061-2080")) +
  ylab("Number of days of exceedance") +
  ggtitle("Mean and standard deviation of number of days\n of likely substandard water quality") +
  scale_color_manual(values=c("#5ab4ac","#d8b365")) +
  scale_fill_manual(values=c("#5ab4ac","#d8b365"))




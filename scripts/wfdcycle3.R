# compute summary stats for 2021 rivers

## load libraries
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)

dat <- read.csv("../dat-orig/lle/WFDRiverCycle3/2021riverscanals.csv", stringsAsFactors = F)

table(dat$WB.type)

## take out canals
dat <- dat[-which(dat$WB.type=="Canal"),]

table(dat$OverallWaterBody, dat$Chem, dat$Eco)

table(dat$OverallWaterBody)
315/nrow(dat)

## focys on ecological not overall for now
good <- dat[which(dat$Eco %in% c("Good", "High")),]

## where are key bio elements not sampled
missing <- good[which(good$Macro_Sub_Conf == '' & good$Inverts_Conf == '' & good$Diatom_Sub_Conf ==''),]
uncertain <- good[which(good$Macro_Sub_Conf == 'Uncertain' & good$Inverts_Conf == 'Uncertain' & good$Diatom_Sub_Conf =='Uncertain'),]

## summarise dat a bit better in terms of the bio elements
biowant <- c('Macro_Sub_Conf', 'Inverts_Conf','Diatom_Sub_Conf')

goodsum <- melt(good, id.vars = c("WBID" ,"WB.name" ), measure.vars = c(biowant))
goodsum$value[which(goodsum$value=="")] <- "Missing"
goodsum$value <- factor(goodsum$value, levels=c("Missing","Uncertain","Quite Certain","Very Certain"))
numval <- data.frame(value=unique(goodsum$value)[order(unique(goodsum$value))], numic=NA)
numval$numic <- c(-1,-0.5,0.5,1)
goodsum <- merge(goodsum, numval, sort = F)
goodsumm <- ddply(goodsum, .(WBID), summarise, totconf=sum(numic) )

ggplot(goodsum, aes(x=WBID, y=variable, fill=value)) +
  theme_bw() +
  geom_tile() +
  scale_fill_manual(values=c('#d7191c','#fdae61','#abd9e9','#2c7bb6'))

ggplot(goodsumm, aes(x=totconf)) +
  theme_bw() +
  geom_bar() 



## plotting malham srp and tp

## load libraries
library(ggplot2)
library(reshape2)
library(extrafont)

font_import()
loadfonts()

## define theme
papertheme <- theme_bw(base_size=12, base_family = 'Arial') +
  theme(legend.position='top')

## read csvs
tp <- read.csv("malham_limnological_tp.csv", stringsAsFactors = F, sep = "\t")
srp <- read.csv("malham_limnological_srp.csv", stringsAsFactors = F, sep="\t")
chl <- read.csv("malham_limnological_chl_a.csv", stringsAsFactors = F, sep="\t")

## melt
tp <- melt(tp, id.vars = "X", value.name = "Total phosphorus")
tp$X <- tolower(tp$X)
srp <- melt(srp, id.vars = "X", value.name="Reactive phosphorus")

chl <- melt(chl, id.vars = "X", value.name="chl")

## combine
both <- merge(tp, srp, sort=F)
all <- merge(both, chl, sort=F)

# make names if melting everything
tp$measure <- "Total phosphorus"
srp$measure <- "Reactive phosphorus"
chl$measure <- "Chlorophyll a"

## plot
pplot <-
ggplot(both, aes(y=`Total phosphorus`, x=`Reactive phosphorus`)) +#, fill=X=="mal")) +
  papertheme +
  #facet_wrap(~X, scales = "free") +
  geom_point(shape=21, size=3, alpha=0.7, fill="grey") +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(breaks = seq(0,80, by=10)) +
  geom_vline(xintercept = c(10,20), linetype="dotted")

ggplot(all, aes(y=`Reactive phosphorus`, x=chl, size=`Total phosphorus`))+
  papertheme +
  xlab("Chlorophyll a") +
  geom_point(shape=21, alpha=0.7, fill="grey") +
  ggtitle("All measures in micrograms per L")

## save plots
ggsave("./R-products/mal-P.jpeg", pplot, height=4, width=6)

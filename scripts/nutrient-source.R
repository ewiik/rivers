## plot nutrients from various sources
## from paper as per file

##libraries
library(ggplot2)

## read file
dat <- read.csv("Edwards&Withers2008Table.csv", stringsAsFactors = F)

## plot file
ggplot(dat, aes(y=median, x=Source, alpha=log(N))) +
  papertheme +
  facet_wrap(~measure, scales="free_y") +
  geom_point() +
  geom_linerange(aes(ymin=lower, ymax=upper)) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

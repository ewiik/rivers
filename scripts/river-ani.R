## animation of hypothetical river....

library(ggplot2)
library(scales)
library(viridis)
library(gganimate)
library(gifski)
library(reshape2)

## create a theme to save linespace in plots
papertheme <- theme_bw(base_size=12, base_family = 'ArialMT') +
  theme(legend.position='top')

## needs to be flowing through a ggplot, within bounds and widening (like river growing)

## data points basically need to be growing so need to add random noise
x <- seq(-10,10, by=.1)
y <- atan(x/2)
bin <- seq(-.1,.1, by=0.01)
xplot <- x+10

yerr <- sample(bin,size=length(y), replace=T)

yalt <- y + yerr*xplot/2

plot(yalt ~ xplot)
lines(y ~ xplot)

## great we have our y and x - now to assign rate of going from blue to green
show_col(viridis_pal()(20))

virpal <- viridis_pal()(20)[3:16] # the limit of change will be an index of virpal
#show_col(virpal)

N <- length(y) # currently 201

highrate <- 12 # the interval along x at which colour change 
medrate <- 30
lowrate <- 80

lowpal <- virpal[1:8]
highpal <- virpal[5:length(virpal)]

## create data frame with all these data
df <- data.frame(y=yalt, x=xplot, chigh=NA, cmed=NA, clow=NA)

df$cext <- rep(highpal, each=highrate)[1:N]
df$chigh <- rep(highpal, each=medrate)[1:N]
df$cmed <- rep(virpal, each=medrate)[1:N]
df$clow <- rep(lowpal, each=lowrate)[1:N]

# any NA means that weren't eough and got to highest value.. so fill with green
if(any(is.na(df$chigh))) {df$chigh[is.na(df$chigh)] <- virpal[length(virpal)]}
if(any(is.na(df$cext))) {df$cext[is.na(df$cext)] <- virpal[length(virpal)]}

dfmelt <- melt(df, measure.vars = c("chigh","cmed","clow","cext"))

ggplot(dfmelt, aes(y=y, x=x, fill=value, col=value, group=value)) +
  papertheme +
  facet_wrap(~variable) +
  geom_point(shape=21, size=5, alpha=0.7) +
  scale_fill_identity() + scale_color_identity() +
  transition_time(x) +
  shadow_wake(wake_length = 0.8, size=2, alpha=0.4) 

ext1 <- ggplot(df, aes(y=y, x=x, fill=cext, col=cext, group=cext)) +
  papertheme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_point(shape=21, size=5, alpha=0.7) +
  scale_fill_identity() + scale_color_identity() +
  ggtitle('Slow river with high nutrients, warmth, sunshine, ...') +
  transition_time(x) +
  shadow_wake(wake_length = 0.8, size=2, alpha=0.4) 

high1 <- ggplot(df, aes(y=y, x=x, fill=chigh, col=chigh, group=chigh)) +
  papertheme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_point(shape=21, size=5, alpha=0.7) +
  scale_fill_identity() + scale_color_identity() +
  ggtitle('Medium-fast river but high nutrients, warmth, sunshine, ...') +
  transition_time(x) +
  shadow_wake(wake_length = 0.8, size=2, alpha=0.4) 

med1 <- ggplot(df, aes(y=y, x=x, fill=cmed, col=cmed, group=cmed)) +
  papertheme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_point(shape=21, size=5, alpha=0.7) +
  scale_fill_identity() + scale_color_identity() +
  ggtitle('Medium-fast river with some nutrients ...') +
  transition_time(x) +
  shadow_wake(wake_length = 0.8, size=2, alpha=0.4) 

low1 <- ggplot(df, aes(y=y, x=x, fill=clow, col=clow, group=clow)) +
  papertheme +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_point(shape=21, size=5, alpha=0.7) +
  scale_fill_identity() + scale_color_identity() +
  ggtitle('Fast river ...') +
  transition_time(x) +
  shadow_wake(wake_length = 0.8, size=2, alpha=0.4) 

animate(ext1, fps = 10, height = 4, width = 6, units="in", res=150) # default
anim_save("./R-products/ext1.gif")
animate(high1, fps = 15, height = 4, width = 6, units="in", res=150) # default
anim_save("./R-products/high1.gif")
animate(med1, fps = 15, height = 4, width = 6, units="in", res=150) # default
anim_save("./R-products/med1.gif")
animate(low1, fps=20, end_pause=10, height = 4, width = 6 , units="in", res=150)
anim_save("./R-products/low1.gif")


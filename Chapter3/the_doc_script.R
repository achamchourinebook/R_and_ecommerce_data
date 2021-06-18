library(dplyr)

xi <- seq(0,1,0.01)

k <- 1
c <- 0.5
lx <- 0.75

yi1 <- k*xi
yi2 <- k*xi^2
yi3 <- (c*xi +xi^2)*k
yi4 <- (xi +xi^2)*k

colours <- viridis(option="D",4)

labels <- data.frame(c=integer(),label=character(),
                     lx=integer(),ly=integer(),colour=character())
labels <- rbind(labels, data.frame(c=1,label="x",
                                   lx=lx,ly=yi1[xi==lx],
                                   colour=colours[1]))
labels <- rbind(labels, data.frame(c=2,label="x^2",
                                   lx=lx,ly=yi2[xi==lx],
                                   colour=colours[2]))
labels <- rbind(labels, data.frame(c=3,label=paste(c,"*x+x^2"),
                                   lx=lx,ly=yi3[xi==lx],
                                   colour=colours[3]))
labels <- rbind(labels, data.frame(c=4,label="x+x^2",
                                   lx=lx,ly=yi4[xi==lx],
                                   colour=colours[4]))

dfi <- data.frame(x=double(),y=double(),c=integer(),colour=character())
dfi1 <- data.frame(x=xi,y=yi1,c=1,colour=colours[1])
dfi2 <- data.frame(x=xi,y=yi2,c=2,colour=colours[2])
dfi3 <- data.frame(x=xi,y=yi3,c=3,colour=colours[3])
dfi4 <- data.frame(x=xi,y=yi4,c=4,colour=colours[4])

dfi <- rbind(dfi, dfi1)
dfi <- rbind(dfi, dfi2)
dfi <- rbind(dfi, dfi3)
dfi <- rbind(dfi, dfi4)

lmod2 <- data.frame(x=xi[xi<0.6], 
                    y=predict(lm(y~x, data=dfi2[dfi2$x<0.6,])),
                    c = 2)
lmod3 <- data.frame(x=xi[xi<0.6], 
                    y=predict(lm(y~x, data=dfi3[dfi3$x<0.6,])),
                    c = 3)
lmod4 <- data.frame(x=xi[xi<0.6], 
                    y=predict(lm(y~x, data=dfi4[dfi4$x<0.6,])),
                    c = 4)

lmod <- rbind(lmod2,lmod3)
lmod <- rbind(lmod,lmod4)

ggplot(dfi, aes(x=x,y=y,colour=factor(c))) +
  geom_line() +
  annotate("text", x=labels$lx, y=labels$ly, 
           label=bquote(.(parse(text=labels$label))),
           colour=labels$colour,
           fontface="bold",
           size=5) +
  scale_colour_manual(values=colours) +
  theme(legend.position="none") +
  geom_line(data=lmod, aes(x=x,y=y,colour=factor(c)))

library(ggplot2)
library(dplyr)
library(viridis)

# start
df.max <- data.frame(x=double(),y=double(),k=double())
df.result <- data.frame(x=double(),y=double(),k=double(),lb=double(),ub=double())

x <- seq(0,1,0.05)

# behaviour function f = x

y <- NA
for(a in seq(1,2.50,0.25))
{
  yprev <- y
  
  y <- -a*x^2 +(a -1)*x
  
  df <- data.frame(x=x,y=y,k=a,lb=yprev,ub=y)
  df <- filter(df, y>=-0.10)
  
  df.result <- rbind(df.result, df)
}

# smoother df.max
for(a in seq(1,2.50,0.01))
{
  xmax <- (a-1)/(2*a)
  ymax <- (a-1)^2/(4*a)
  df.max <- rbind(df.max, data.frame(x=xmax,y=ymax,k=a))
}

slice_head(df.result, n=10)
slice_tail(df.result, n=10)
str(df.max)

theme <- theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 10),
               plot.caption = element_text(face = "italic"),
               axis.title = element_text(face = "italic", size = 9)
)

ggplot(df.result) +
  geom_line(aes(x=x,y=y, colour=factor(k))) +
  geom_ribbon(aes(x=x,ymin=lb,ymax=ub, fill=factor(k)), alpha=0.5) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha=0.5) +
  scale_colour_viridis(option = "D", discrete = TRUE, alpha=1) +
  labs(x="Discount", y="Profit over regular sales",
       title="Promotional sales performance",
       subtitle = "Linear behaviour function",
       fill="PAF", colour="PAF")+
  scale_y_continuous(breaks = seq(-0.10,0.5,0.05)) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  geom_line(data=df.max, aes(x=x,y=y)) +
  geom_segment(x=0,y=0,xend=1,yend=0, colour="red", size=0.1) +
  theme 

# promos
source("Modules/dbData.R")

df.promos <- db.data(vw="vwPromos")
str(df.promos)

df.promos$pp <- df.promos$PromoUnitPrice*df.promos$PromoUnitsPerDay/
  (df.promos$RegUnitPrice*df.promos$RegUnitsPerDay) -1

df.promos$disc <- (df.promos$RegUnitPrice-df.promos$PromoUnitPrice)/df.promos$RegUnitPrice

df.promos$sku <- paste0(df.promos$PromotionKey,"-",df.promos$ProductKey)

# install.packages("ggrepel", dependencies = TRUE)
# install.packages("ggnewscale", dependencies = TRUE)

library(ggnewscale)
library(ggrepel)

ggplot(df.result) +
  geom_line(aes(x=x,y=y, colour=factor(k))) +
  geom_ribbon(aes(x=x,ymin=lb,ymax=ub, fill=factor(k)), alpha=0.5) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha=0.5) +
  scale_colour_viridis(option = "D", discrete = TRUE, alpha=1) +
  labs(x="Discount", y="Profit over regular sales",
       title="Promotional sales performance",
       subtitle = "Linear behaviour function",
       fill="PAF", colour="PAF")+
  scale_y_continuous(breaks = seq(-0.20,0.5,0.05)) +
  scale_x_continuous(breaks = seq(0,1,0.10)) +
  geom_line(data=df.max, aes(x=x,y=y)) +
  geom_segment(x=0,y=0,xend=1,yend=0, colour="red", size=0.1) +
  new_scale_color() +
  geom_point(data=df.promos, aes(x=disc,y=pp, colour=factor(PromotionKey)))+
  labs(colour="Promo") +
  scale_colour_viridis(option = "C", discrete = TRUE, alpha=1) +
  geom_label_repel(data=df.promos,x=df.promos$disc,y=df.promos$pp, label=df.promos$sku) +
  theme 

### significance
x <- seq(-3,3,0.01)
y <- dt(x=x,df=3)

x.a <- seq(2,3,0.01)
y.a <- dt(x=x.a,df=3)

df.t <- data.frame(x=x,y=y)
str(df.t)

df.area <- data.frame(x=x.a, y=y.a)
str(df.area)

ggplot(df.t, aes(x=x,y=y)) +
  geom_line() +
  geom_vline(xintercept = 2, colour = "blue") +
  geom_area(data = df.area, aes(y=y), alpha=0.5, fill = "blue")

###
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)

source("Modules/dbData.R")

df.data <- db.data(vw="vwPromos2")
str(df.data)

df.data$sku <- paste0(df.data$PromotionKey,"-",df.data$ProductKey)

df.promos2 <- df.data %>% 
  distinct(PromotionKey, ProductKey) %>% 
  filter(PromotionKey!=1)
str(df.promos2)

df.promos2$sku <- paste0(df.promos2$PromotionKey,"-",df.promos2$ProductKey) 
df.promos2$pvalue <- 1.00
df.promos2$m <- 0.00

for(i in 1:count(df.promos2)$n)
{
  s <- df.promos2[i,"sku"]
  prod <- df.promos2[i,"ProductKey"]
  
  df.temp.p <- filter(df.data, sku==s)
  df.temp.r <- filter(df.data, PromotionKey==1 & ProductKey==prod)
  
  t <- t.test(df.temp.p$Units, df.temp.r$Units)
  p <- t$p.value
  m.p <- mean(df.temp.p$Units)
  m.r <- mean(df.temp.r$Units)
  d.r <- sd(df.temp.r$Units)
  m <- (m.p-m.r)/m.r
  #m <- t$statistic
  
  df.promos2[i,"pvalue"] <- p/2
  df.promos2[i, "m"] <- m
}

theme <- theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 10),
               plot.caption = element_text(face = "italic"),
               axis.title = element_text(face = "italic", size = 9)
)

ggplot(df.promos2, aes(x=pvalue,y=m,colour=factor(PromotionKey))) +
  geom_point() +
  labs(colour="Promo", 
       x="Probability to see by chance",
       y="Normalized means difference",
       title="Promotional sales increase significance") +
  geom_label_repel(data=df.promos2,x=df.promos2$pvalue,y=df.promos2$m, 
                   label=df.promos2$sku,colour="black") + 
  scale_colour_viridis(option = "C", discrete = TRUE, alpha=1) +
  theme +
  geom_segment(x=0.05,y=0,xend=0.05,yend=2, colour="red", size=0.1) +
  scale_x_continuous(limits = c(0,0.5))




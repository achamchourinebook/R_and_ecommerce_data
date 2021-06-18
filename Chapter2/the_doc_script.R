library(dplyr) 
library(lubridate) 
library(ggplot2)

source("Modules/dbData.R")

p <- db.data(db="BrazilianEcommerce",vw="vwPrice")
str(p)

sum(p$cnt) 
max(p$cnt) 
max(p$price) 

quantile(p$cnt, c(0.50,0.68,0.95,0.99))

pc <- group_by(p, sku) %>% summarise(cnt=sum(cnt)) %>% select(sku, cnt)
quantile(pc$cnt, c(0.50,0.68,0.95,0.99))

###
ggplot() +
  geom_histogram(data=pc, aes(x=cnt), colour="black", fill="steelblue") +
  labs(title = "Basic quantity distribution", 
       subtitle = "Default # of bins (30)") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(face = "italic"),
        axis.title = element_text(face = "italic", size = 9)
  )

theme <- theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 10),
               plot.caption = element_text(face = "italic"),
               axis.title = element_text(face = "italic", size = 9)
)

ggplot() +
  geom_histogram(data=pc, aes(x=cnt), 
                 colour="black", fill="steelblue", 
                 binwidth = 1, center = 1) +
  scale_x_continuous(limits=c(0,20), breaks = seq(0,20,1)) +
  labs(title = "Quantity distribution, limited scale", 
       subtitle = "20 bins") +
  theme

ggplot(pc, aes(x=cnt)) +
  geom_density(colour="red", stat="count") +
  scale_x_continuous(limits=c(0,20), breaks = seq(0,20,1)) +
  labs(title="Density function, stat='count'", subtitle="Limited scale") +
  theme

ggplot(pc, aes(x=cnt)) +
  geom_histogram(colour="black", fill="steelblue", alpha=0.5,
                 binwidth = 1, center=1) +
  geom_density(colour="red", stat="count", size=1) +
  scale_x_continuous(limits=c(0,20), breaks = seq(0,20,1)) +
  labs(title = "Quantity distribution, limited scale", 
       subtitle = "Histogram and density curve") +
  theme

p1000 <- sample_n(p, 1000)
str(p1000)

ggplot(p1000, aes(x=price,y=cnt)) +
  geom_point() +
  labs(title = "Random sample of 1000") +
  theme

p500 <- sample_n(p, 500)
str(p500)

ggplot(p500, aes(x=price,y=cnt)) +
  geom_point() +
  labs(title = "Random sample of 500") +
  theme

histogram.agg <- function(data, bins)
{
  # builds a histogram for pre-aggregated data
  # data is data.frame with columns x and y
  # x is a variable for which we are generating the distribution
  # y is the pre-aggregated counts of x
  # bins is a vector of bin's boundaries.
  
  # function returns data.frame with the foloowing columns:
  #   value:  mid value of the bin boundaries
  #   f:      left bin boundary
  #   t:      right bin boundary
  #   sum:    sum(y) - this is what regular hist reports as frequency
  
  #   in addition to that it will return a few more values
  #   mean:   mean(y)
  #   sd:     standard deviation of y
  #   cnt:    number of data points inside the bin
  #   min:    min(y)
  #   max:    max(y)
  #   sd2:    2 standard deviations of y
  
  require(dplyr)
  
  f <- bins[1:(length(bins)-1)]
  length(f)
  t <- bins[2:(length(bins))]
  
  bins.df <- data.frame(f, t, bjoin=TRUE)
  
  # add bjoin column to imitate all-on-all joining, 
  # as dplyr join does not support expressions
  # we could do it with merge, but dplyr is significantly faster
  data$bjoin <- TRUE
  
  d <- inner_join(data, bins.df, by=c("bjoin"="bjoin")) %>%
    filter(x > f & x <= t) %>%
    group_by(t,f) %>%
    summarise(sum=sum(y), mean = mean(y), sd = sd(y), cnt = n(), min = min(y), max = max(y)) %>%
    mutate(sd2 = mean+2*sd, value=(f+t)/2) %>%
    select(value, sum, mean, sd, cnt, min, max, sd2, f, t)
  
  return(d)
}

source("Modules/histogram.R")

df <- data.frame(x=p$price,y=p$cnt)

bns <- seq(0,6800,20)
d <- histogram.agg(df, bns)

ggplot(d, aes(x=value,y=sum)) +
  geom_col(colour="black", fill="steelblue", alpha=0.5) +
  labs(title = "Price histogram, full scale", 
       subtitle = "Fixed size bin, 340 bins",
       y="Total sales counts") +
  theme

bns <- c(seq(0,500,20),seq(1000,7000,500))
d <- histogram.agg(df, bns)

ggplot() +
  geom_line(data=d, aes(x=value,y=sum), colour="red", linetype="solid") +
  labs(title = "Price vs total sales counts", 
       subtitle = "Variable size bins, sharp size change",
       y="Total sales counts") +
  theme

bns <- c(seq(0,600,20),seq(650,900,50),seq(1000,1500,100),seq(2000,7000,500))
d <- histogram.agg(df, bns)

ggplot() +
  geom_line(data=d, aes(x=value,y=sum), colour="red", linetype="solid") +
  labs(title = "Price vs total sales counts", 
       subtitle = "Variable size bins, smooth size change",
       y="Total sales counts") +
  theme

ggplot(d, aes(x=value,y=sum)) +
  geom_col(colour="black", fill="steelblue", alpha=0.5) +
  labs(title = "Price histogram, full scale", 
       subtitle = "Variable size bins, smooth size change",
       y="Total sales counts") +
  theme

ggplot(d, aes(x=value,y=sum)) +
  geom_col(colour="black", fill="steelblue", alpha=0.5) +
  scale_x_continuous(limits=c(0,500), breaks = seq(0,500,50), minor_breaks = seq(10,500,10)) +
  labs(title = "Price histogram, limited scale", 
       subtitle = "Bin width 20",
       y="Total sales counts") +
  theme

bns <- seq(0,500,10)
d <- histogram.agg(df, bns)

ggplot(d, aes(x=value,y=sum)) +
  geom_col(colour="black", fill="steelblue", alpha=0.5) +
  scale_x_continuous(limits=c(0,500), breaks = seq(0,500,50), minor_breaks = seq(10,500,10)) +
  labs(title = "Price histogram, limited scale", 
       subtitle = "Bin width 10",
       y="Total sales counts") +
  theme

# mean with sd interval
smean <- predict(loess(mean~value,d), d$value)
ssd.l <- predict(loess(mean-sd~value,d), d$value)
ssd.u <- predict(loess(mean+sd~value,d), d$value)
d.smoothed <- data.frame(price=d$value, lb=ssd.l, ub=ssd.u, mean=smean)

ggplot(d.smoothed) +
  geom_line(aes(x=price,y=mean), colour="red") +
  geom_line(aes(x=price,y=ifelse(lb<0,0,lb)), colour="steelblue") +
  geom_line(aes(x=price,y=ub), colour="steelblue") +
  geom_ribbon(aes(x=price,ymin=ifelse(lb<0,0,lb),ymax=ub), alpha=0.3, fill="blue") +
  labs(title = "Mean with 68% interval", 
       subtitle = "Full scale",
       y="Sales counts") +
  theme

###
# simulate observed sales
sales.true <- data.frame(product=rep(c("p1","p2","p3"),each=5), 
                         price=c(rep(10.00,5),
                                 c(15.00,22.00,22.00,22.00,22.00),
                                 c(19.00,27.00,27.00,31.00,31.00)))
sales.true
sales.m <- sales.true %>% group_by(product) %>% summarise(m=sum(price)/n())

# mean
sales.mean <- inner_join(sales.true, sales.m, by=c("product"="product")) %>% 
  mutate(price=m) %>%
  select(product, price)

# add type
sales.true$price.type <- "Observed"
sales.true$h <- "1"
sales.mean$price.type <- "WeightedMean"
sales.mean$h <- "2"

# combine two
sales.both <- rbind(sales.mean, sales.true)
sales.both$h <- "3"

# bind all three together
sales.all <- rbind(sales.mean, sales.true, sales.both)

# note "h" is an aesthetics name that will be used to draw facets
labels <- labeller(h=c('1'="Observed",'2'="WeightedMean/'Original'",'3'="Overlap"))

# text label, note only one h value will be used (out of 3)
txt <- data.frame(label="Promotional sale",
                  h=factor(3, levels=c(1,2,3)),
                  x=27,
                  y=5.8) 

# curvature (arrow), note only one h value will be used (out of 3)
crv <- data.frame(h=factor(3, levels=c(1,2,3)),
                  x=30,
                  y=4.5,
                  xend=10,
                  yend=5.5) 

# note the use of facet_grid() and geom_curve()
ggplot() +
  geom_histogram(data=sales.all, aes(x=price, fill=price.type), 
                 colour="black", alpha=0.5, 
                 binwidth=10,
                 position="identity") +
  scale_x_continuous(breaks = seq(0,50,10)) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  facet_grid(cols=vars(h), labeller = labels) +
  scale_fill_manual(values=c("red","steelblue")) +
  geom_curve(data=crv, 
             aes(x=x,y=y,xend=xend,yend=yend), 
             colour="red", size=1, 
             curvature=0.3, arrow=arrow()) +
  geom_text(data=txt, aes(x=x,y=y,label=label), colour="red", size=6, angle=0)

bns <- c(seq(0,600,20),seq(650,900,50),seq(1000,1500,100),seq(2000,7000,500))

df <- data.frame(x=p$price,y=p$cnt)
str(df)

d <- histogram.agg(df, bns)
str(d)

dfm <- data.frame(x=pm$price,y=pm$cnt)
str(dfm)

dm <- histogram.agg(dfm, bns)
str(dm)

d$price.type <- "Observed"
dm$price.type <- "WeightedMean"
da <- rbind(d, dm)

ggplot(da, aes(x=value,y=sum)) +
  geom_col(colour="black", aes(fill=price.type), alpha=0.5, position="identity") +
  scale_x_continuous(limits=c(0,500), 
                     breaks = seq(0,500,50), minor_breaks = seq(10,500,10)) +
  scale_fill_manual(values=c("red", "steelblue")) +
  labs(title = "Observed and WeightedMean ('Original') overlap", 
       subtitle = "Limited scale",
       y="Total sales counts",
       fill="Price Type"
  ) +
  theme

###
t5var <- inner_join(p,pm,by=c("sku"="sku"),suffix=c("",".mean")) %>%
  group_by(sku) %>%
  summarise(v=sum((price.mean-price)^2)/n(), m=min(price.mean)) %>%
  select(sku, v, m) %>%
  arrange(desc(v)) %>%
  top_n(5, v) # << top 5 data points only
t5var

t5p <- inner_join(p, t5var, by=c("sku"="sku")) %>%
  arrange(sku,price, cnt) %>%
  select(sku, cnt, price, m)

# combine mean and true prices
t5p1 <- mutate(t5p, price.type="Observed") %>% select(sku, cnt, price, price.type)
t5p2 <- t5p %>% group_by(sku) %>% summarise(price=min(m),cnt=sum(cnt)) %>%
  mutate(price.type="WeightedMean") %>% 
  select(sku, cnt, price, price.type)
t5p2

# "de-aggregate" the counts to use results in hist function
t5p1 <- as.data.frame(lapply(t5p1, rep, t5p1$cnt))
t5p1
t5p2 <- as.data.frame(lapply(t5p2, rep, t5p2$cnt))
t5p2

t5pc <- rbind(t5p1, t5p2)

ggplot() +
  geom_histogram(data=t5pc, aes(x=price, fill=price.type), colour="black", alpha=0.5) +
  facet_wrap(vars(sku)) +
  scale_y_continuous(breaks=seq(0,100,1)) +
  labs(fill="Price Type") +
  scale_fill_manual(values=c("red", "steelblue")) +
  labs(y="Total sales counts") +
  theme  

pm.j <- mutate(pm, b=TRUE)
d.j <- mutate(d, b=TRUE)

# top 5%
top5p <- inner_join(pm.j, d.j, by=c("b"="b"), suffix=c("",".bin")) %>%
  filter(price>f & price<=t & cnt > sd2) %>%
  select(sku, price, cnt, sd2, f, t)

# first 10
topp <- mutate(top5p, delta=cnt-sd2) %>% arrange(desc(delta)) %>% 
  select(sku, price, cnt, sd2, delta) %>%
  slice_head(n=10)
topp

smean <- predict(loess(mean~value,d), d$value)
ssd.l <- predict(loess(mean-sd*2~value,d), d$value)
ssd.u <- predict(loess(mean+sd*2~value,d), d$value)
d.smoothed <- data.frame(price=d$value, lb=ssd.l, ub=ssd.u, mean=smean)

ggplot(d.smoothed) +
  geom_point(data=topp, aes(x=price,y=cnt, colour=sku,shape=sku), size=3) +
  scale_shape_manual(values=seq(1:10)) +
  geom_line(aes(x=price,y=mean), colour="red") +
  geom_line(aes(x=price,y=ifelse(lb<0,0,lb)), colour="steelblue") +
  geom_line(aes(x=price,y=ub), colour="steelblue") +
  geom_ribbon(aes(x=price,ymin=ifelse(lb<0,0,lb),ymax=ub), alpha=0.3, fill="blue") +
  scale_x_continuous(limits = c(0,1000), breaks=seq(0,1000,50), minor_breaks = seq(0,1000,10)) +
  labs(title = "10 best performing products", 
       subtitle = "Comparing to 95% boundary (blue)",
       y="Sales counts",
       colour="Product",
       shape="Product") +
  theme

top5p50 <- mutate(top5p, delta=cnt-sd2) %>% arrange(desc(delta)) %>% 
  select(sku, price, cnt, sd2, delta) %>%
  slice_head(n=50)

ggplot() +
  geom_col(data=top5p50, aes(x=sku, y=cnt), position="identity", colour="black", fill="steelblue") +
  labs(title = "50 best performing products", 
       y="Sales counts",
       x="product") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) +
  theme

ggplot() +
  geom_col(data=top5p50, aes(x=reorder(sku,-cnt), y=cnt, fill=price), position="identity", colour="black") +
  labs(title = "50 best performing products", 
       subtitle = "Auto colours",
       y="Total sales counts",
       x="product",
       fill="Price") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) +
  theme

install.packages("viridis", dependencies = TRUE)

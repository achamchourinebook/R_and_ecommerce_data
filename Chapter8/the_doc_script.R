cal <- seq(as.Date("2017-01-01"), as.Date("2019-12-31"),1)
df.sim <- data.frame(date = cal, 
                     dom = mday(cal), 
                     dow = wday(cal), 
                     mon = month(cal),
                     week = isoweek(cal),
                     year = year(cal),
                     x=1:length(cal))

level <- max(df.sim$x) +df.sim$x
level <- level/mean(level)

set.seed(0)
noise <- rnorm(n=df.sim$x, mean=0, sd=0.1)

df.sim$y <- level +noise

# weekend values are down (by 50%)
df.sim$y <- ifelse(df.sim$dow==6, df.sim$y*0.8, df.sim$y) # fri
df.sim$y <- ifelse(df.sim$dow==7, df.sim$y*0.6, df.sim$y) # sat
df.sim$y <- ifelse(df.sim$dow==1, df.sim$y*0.5, df.sim$y) # sun

# monthly pattern: end of the month sales are up (by 50%)
df.sim$y <- ifelse(df.sim$dom==27 & df.sim$mon==2, df.sim$y*1.4, df.sim$y)
df.sim$y <- ifelse(df.sim$dom==28 & df.sim$mon==2, df.sim$y*1.5, df.sim$y)

df.sim$y <- ifelse(df.sim$dom==29, df.sim$y*1.4, df.sim$y)
df.sim$y <- ifelse(df.sim$dom==30, df.sim$y*1.5, df.sim$y)
df.sim$y <- ifelse(df.sim$dom==31, df.sim$y*1.4, df.sim$y)

# yearly pattern (Holiday season Nov 15 - Dec 20) 100 % increase
df.sim[df.sim$mon==11 & df.sim$dom>=15 & df.sim$dom<=30, "y"] <-
  df.sim[df.sim$mon==11 & df.sim$dom>=15 & df.sim$dom<=30, "y"]*1.7
df.sim[df.sim$mon==12 & df.sim$dom>=1 & df.sim$dom<=20, "y"] <-
  df.sim[df.sim$mon==12 & df.sim$dom>=1 & df.sim$dom<=20, "y"]*2

theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 10),
  plot.caption = element_text(face = "italic"),
  axis.title = element_text(face = "italic", size = 9)
)

ggplot(df.sim) +
  geom_line(aes(x=date,y=y)) +
  scale_x_date(date_breaks="1 month", date_labels="%b %d, %y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Simulated data, 2017-2019")

ggplot(df.sim) +
  geom_line(aes(x=date,y=y)) +
  scale_x_date(date_breaks="1 month", date_minor_breaks="1 week", 
               date_labels="%b %d", 
               limits=c(as.Date("2018-02-25"),as.Date("2018-05-26"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank()) +
  theme +
  labs(title="Simulated data, 2017-2019", subtitle="Zoomed in")

ggplot(df.sim) +
  geom_line(aes(x=date,y=y)) +
  scale_x_date(date_breaks="1 day", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2018-02-25"),as.Date("2018-03-26"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Simulated data, 2017-2019", subtitle="Zoomed in")

###
xpr <- 1:(max(df.sim$x)+364)

lpr1 <- lm(y~x, df.sim)
pr1 <- predict(lpr1, data.frame(x=xpr))
df.pr1 <- data.frame(x=xpr,y=pr1)

lpr2 <- loess(y~x,df.sim, span=0.5, control=loess.control(surface="direct"))
pr2 <- predict(lpr2, data.frame(x=xpr))
df.pr2 <- data.frame(x=xpr,y=pr2)

lpr3 <- loess(y~x,df.sim, span=0.9, control=loess.control(surface="direct"))
pr3 <- predict(lpr3, data.frame(x=xpr))
df.pr3 <- data.frame(x=xpr,y=pr3)

ggplot(df.sim) +
  geom_line(aes(x=x,y=y)) +
  theme +
  labs(title="Simulated data, forecast for the next 365 days", x="Days") +
  geom_line(data=df.pr1, aes(x=x,y=y), colour="red", size=1) +
  geom_line(data=df.pr2, aes(x=x,y=y), colour="blue", size=1) +
  geom_line(data=df.pr3, aes(x=x,y=y), colour="yellow", size=1)

# with confidence and prediction interval

lpr1 <- lm(y~x, df.sim)
pr1 <- predict(lpr1, data.frame(x=xpr), interval="prediction")
dfi.pr1 <- data.frame(x=xpr,y=pr1[,"fit"], pi_min=pr1[,"lwr"], pi_max=pr1[,"upr"])

pr1 <- predict(lpr1, data.frame(x=xpr), interval="confidence")
dfi.pr1 <- cbind(dfi.pr1, ci_min=pr1[,"lwr"], ci_max=pr1[,"upr"])
str(dfi.pr1)

dfi.pr1 <- filter(dfi.pr1,x>length(xpr)-365)

ggplot(df.sim) +
  geom_line(aes(x=x,y=y)) +
  labs(title="Simulated data, forecast for the next 365 days", x="Days") +
  theme +
  geom_line(data=dfi.pr1, aes(x=x,y=y), colour="blue", size=0.8) +
  geom_line(data=dfi.pr1, aes(x=x,y=ci_min), 
            colour="blue", size=1, alpha=0.4, size=0.5) +
  geom_line(data=dfi.pr1, aes(x=x,y=ci_max), 
            colour="blue", size=1, alpha=0.4, size=0.5) +
  geom_ribbon(data=dfi.pr1, aes(x=x, ymin=ci_min, ymax=ci_max), 
              fill="blue", alpha=0.4) +
  geom_line(data=dfi.pr1, aes(x=x,y=pi_min), 
            colour="steelblue", size=1, alpha=0.4, size=0.5) +
  geom_line(data=dfi.pr1, aes(x=x,y=pi_max), 
            colour="steelblue", size=1, alpha=0.4, size=0.5) +
  geom_ribbon(data=dfi.pr1, aes(x=x, ymin=pi_min, ymax=pi_max), 
              fill="steelblue", alpha=0.4)

###
# mean med & sd
sim.mean <- mean(df.sim$y)
sim.med <- median(df.sim$y)
sim.sd <- sd(df.sim$y)
sim.sd.mean <- sd(df.sim$y)/sim.mean
sim.sd.med <- sd(df.sim$y)/sim.med
# detrend
m <- lm(y~x, df.sim)
p <- predict(m, data.frame(x=df.sim$x))

df.sim$t <- p
df.sim$ydt <- df.sim$y -df.sim$t

# weekly pattern
sim.g.w <- df.sim %>% group_by(dow) %>% 
  summarise(y=(sum(ydt)/n())/sim.med, n=n()) %>%
  mutate(y=y*100)

ggplot(sim.g.w) +
  geom_col(aes(x=dow,y=y), position="identity", fill="steelblue", alpha=0.8) +
  scale_x_continuous(labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
                     breaks = seq(1,7,1)) +
  labs(x="DoW",y="Deviation, % of median",
       title="Weekly pattern",
       caption="Red dotted lines 1 standard deviation of original data, % of median") +
  geom_hline(yintercept=sim.sd.med*100, colour="red", linetype="dotted") +
  geom_hline(yintercept=-sim.sd.med*100, colour="red", linetype="dotted") +
  theme

# adjust for weekly 
sim.g.w1 <- df.sim %>% group_by(dow) %>% summarise(y=(sum(ydt)/n()), n=n())

df.tmp <- left_join(df.sim, sim.g.w1, by=c("dow"="dow")) %>%
  mutate("ydt-w"=ydt-y.y, w=y.y) %>%
  select(w, "ydt-w")

df.sim <- cbind(df.sim, df.tmp)
str(df.sim)

sim.g.m.ad <- df.sim %>% 
  group_by(dom) %>% summarise(y=(sum(`ydt-w`)/n())/sim.med, n=n()) %>%
  mutate(y=y*100)

ggplot(sim.g.m.ad) +
  geom_col(aes(x=dom,y=y), position="identity", fill="steelblue", alpha=0.8) +
  scale_x_continuous(labels = seq(1,31,1),
                     breaks = seq(1,31,1)) +
  labs(x="DoM",y="Deviation, % of median",
       title="Monthly pattern with adjustment",
       caption="Red dotted lines 1 standard deviation of original data, % of median") +
  geom_hline(yintercept=sim.sd.med*100, colour="red", linetype="dotted") +
  geom_hline(yintercept=-sim.sd.med*100, colour="red", linetype="dotted") +
  theme

sim.g.m1 <- df.sim %>% 
  group_by(dom) %>% summarise(y=(sum(`ydt-w`)/n()), n=n())

df.tmp <- left_join(df.sim, sim.g.m1, by=c("dom"="dom")) %>%
  mutate("ydt-w-m"=`ydt-w`-y.y, m=y.y) %>%
  select("ydt-w-m", m)

df.sim <- (df.sim[,seq(1,12)])

df.sim <- cbind(df.sim, df.tmp)
str(df.sim)

sim.g.y <- group_by(df.sim, year, week) %>% summarise(y=sum(`ydt-w-m`), n=n()) %>%
  mutate(m=y/n) %>%
  group_by(week) %>% summarise(y=sum(m)/n()) %>%
  mutate(y=(y)/sim.med *100)

ggplot(sim.g.y) +
  geom_col(aes(x=week,y=y), fill="steelblue", alpha=0.9) +
  scale_x_continuous(labels = seq(0,52,5),
                     breaks = seq(0,52,5)) +  
  labs(x="Week", y="Deviation, % of median",
       title="Yearly Pattern",
       subtitle="Adjusted",
       caption="Red dotted lines 1 standard deviation of original data, % of median") +
  geom_hline(yintercept=sim.sd.med*100, colour="red", linetype="dotted") +
  geom_hline(yintercept=-sim.sd.med*100, colour="red", linetype="dotted") +
  theme

sim.g.y1 <- df.sim %>% 
  group_by(year, week) %>% summarise(y=sum(`ydt-w-m`), n=n()) %>%
  mutate(m=y/n) %>%
  group_by(week) %>% summarise(y=sum(m)/n())

df.tmp <- left_join(df.sim, sim.g.y1, by=c("week"="week")) %>%
  mutate(yr=y.y) %>%
  select(yr)

df.sim <- cbind(df.sim, df.tmp)

# compare predicted and true values (hindcast)
ggplot(df.sim) +geom_line(aes(x=date,y=y)) +
  geom_line(aes(x=date,y=t+w+m+yr), colour="blue") +
  scale_x_date(date_breaks="1 month", 
               date_labels="%b %d, %a") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Naive Prediction", subtitle="Hindcast")

# residuals
res <- df.sim$y -(df.sim$t+df.sim$w+df.sim$m+df.sim$yr)
res.var <- (sd(res))^2

# forecast
fi <- (length(df.sim$x)+1):(length(df.sim$x)+35)

m <- lm(y~x,df.sim)

# degrees of freedom
deg.free <- m$df.residual

pr <- predict(m, data.frame(x=fi), se.fit=TRUE)
pr.df <- data.frame(x=1:length(pr[["fit"]]), 
                    date=seq(as.Date("2020-01-01"),
                             as.Date("2020-01-01")+length(pr[["fit"]])-1,1),
                    p=pr[["fit"]], se=pr[["se.fit"]])

# calc the prediction interval
cvalue <- qt((1-0.95)/2, deg.free)
pred.i <- cvalue*(sqrt((pr.df$se)^2 +res.var))
pr.df$dow <- wday(pr.df$date)
pr.df$dom <- mday(pr.df$date)
pr.df$week <- isoweek(pr.df$date)

# prepare forecast adj
wa.df <- group_by(df.sim, dow) %>% summarise(wa=sum(w)/n(), n()) %>% 
  select(dow, wa)

ma.df <- group_by(df.sim, dom) %>% summarise(ma=sum(m)/n()) %>%
  select(dom, ma)

ya.df <- group_by(df.sim, week) %>% summarise(ya=sum(yr)/n()) %>%
  select(week, ya)

pr.df <- left_join(pr.df, wa.df, by=c("dow"="dow")) %>%
  select(x,date,p,dow,dom,week,wa)

pr.df <- left_join(pr.df, ma.df, by=c("dom"="dom")) %>%
  select(x,date,p,dow,dom,week,wa,ma)

pr.df <- left_join(pr.df, ya.df, by=c("week"="week")) %>%
  select(x,date,p,dow,dom,week,wa,ma,ya)

pr.df$pn <- pr.df$p +pr.df$wa +pr.df$ma +pr.df$ya 

pr.df$lwr <- pr.df$pn +pred.i
pr.df$upr <- pr.df$pn -pred.i

ggplot(df.sim) +geom_line(aes(x=date,y=y)) +
  geom_line(data=pr.df, aes(x=date,y=pn), colour="blue") +
  
  # with uncertainty
  geom_line(data=pr.df, aes(x=date,y=lwr), 
            colour="blue", alpha=0.3) +
  geom_line(data=pr.df, aes(x=date,y=upr), 
            colour="blue", alpha=0.3) +
  geom_ribbon(data=pr.df, aes(x=date, ymin=lwr, ymax=upr), 
              fill="blue", alpha=0.3) +  
  
  scale_x_date(date_breaks="1 month", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2019-09-01"),as.Date("2020-02-04"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Naive Prediction", subtitle="Zoomed in")

###
library(forecast) 

ts <- msts(df.sim$y, seasonal.periods=c(7,30,365))
mstl.m <- mstl(ts) # this function requires 2 full periods +1 observation! 

autoplot(mstl.m)

f <- forecast(mstl.m, h=35)
df.f <- as.data.frame(f)
str(df.f)

df.f$date = seq(as.Date("2020-01-01"),as.Date("2020-01-01")+34,1)

ggplot(df.sim) +geom_line(aes(x=date,y=y)) +
  geom_line(data=df.f, aes(x=date,y=`Point Forecast`), colour="blue") +
  
  geom_line(data=df.f, aes(x=date,y=`Lo 95`), colour="steelblue") +
  geom_line(data=df.f, aes(x=date,y=`Hi 95`), colour="steelblue") +
  geom_ribbon(data=df.f, aes(x=date,ymin=`Lo 95`,ymax=`Hi 95`), alpha=0.3, fill="blue") +
  
  scale_x_date(date_breaks="1 month", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2019-09-01"),as.Date("2020-02-04"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="MSTL Prediction", 
       subtitle="Zoomed in",
       caption="With 95% confidence interval")

###
# HW wrong periodicity
prd <- 5
ts1 <- msts(df.sim$y, seasonal.periods=c(prd))

m <- HoltWinters(ts1)
f1 <- forecast(m, h=35)
df.f1 <- as.data.frame(f1)
df.f1$date = seq(as.Date("2020-01-01"),as.Date("2020-01-01")+34,1)

str(df.f1)

ggplot(df.sim) +
  geom_line(aes(x=date,y=y)) +
  geom_line(data=df.f1, aes(x=date,y=`Point Forecast`), colour="blue") +
  scale_x_date(date_breaks="1 month", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2019-09-01"),as.Date("2020-02-04"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="HW Prediction", 
       subtitle=paste0(ifelse(prd==7,"Right","Wrong"), " weekly periodicity"))

# stl, one period
ts1 <- msts(df.sim$y, seasonal.periods=c(7))
mstl.m1 <- mstl(ts1) # this function requires 2 full periods +1 observation! 
autoplot(mstl.m1)

###
# acf
ts1 <- msts(df.sim$y, seasonal.periods=c(7))
mstl.m1 <- mstl(ts1)

m.df <- as.data.frame(mstl.m1)
acf(m.df$Data, lag.max=30)
acf(m.df$Trend, lag.max=100)

# remove yearly influence
m.df1 <- slice_head(m.df, n=310)
acf(m.df1$Trend, lag.max=100)

###
x <- 0:9
y0 <- sin(2*pi*x/5) +1
y1 <- c(1,1.9,1.25,0.5,0.1,1,1.9,1.25,0.5,0.1)
y2 <- c(0,1.8,0,0,0,0,1.8,0,0,0)

y3 <- c(0,0,0,1,1,1,1,0,0,0)
y4 <- sin(2*pi*x/20)
y5 <- sin(2*pi*x/11)

l <- list()
l$sin <- data.frame(x=x,y=y0)
l$period1 <- data.frame(x=x,y=y1)
l$period2 <- data.frame(x=x,y=y2)
l$pulse <- data.frame(x=x,y=y3)
l$half <- data.frame(x=x,y=y4)
l$halfplus <- data.frame(x=x,y=y5)

ll <- length(l)
l.new <- list()

for(j in 1:ll)
{
  name <- names(l[j])
  
  df <- l[[j]]
  
  p1 <- ggplot(df) +geom_line(aes(x=x,y=y)) +
    scale_x_continuous(breaks = seq(0,10,1)) +
    labs(title=name)
  
  N <- length(df$y)
  N2 <- floor(N/2)
  ft <- fft(df$y)
  i <- Mod(ft)
  a <- i[2:(N2+1)]
  f <- seq(from=1/N,by=1/N,length.out = N2)
  
  dft <- data.frame(f=f,a=a)
  p2 <- ggplot(dft) +geom_col(aes(x=f,y=a), width=0.005, orientation="x") +
    scale_x_continuous(breaks=seq(0,0.5,0.1)) +
    labs(title=" ")
  
  l.new[[name]] <- list("p1"=p1,"p2"=p2)
}

p <- ggplot() +coord_fixed(xlim=c(0,4),ylim=c(0,3))

for(j in 1:ll)
{
  i <- ll-j +1
  h <- ((j-1) %/% 3)*2
  v <- ((i-1) %% 3)
  
  print(paste0(names(l.new[j]),",",v,",",i))
  
  p <- p +  
    annotation_custom(ggplotGrob(l.new[[j]]$p1), 
                      xmin=0+h, xmax=1+h, ymin=v, ymax=v+1) +
    annotation_custom(ggplotGrob(l.new[[j]]$p2), 
                      xmin=1+h, xmax=2+h, ymin=v, ymax=v+1)
}

p +theme_void() +theme(plot.title = element_text(size = 8, face = "bold"))  

spct <- spectrum(df.sim$y, log="no")

spct.df <- data.frame(freq=spct$freq, spec=spct$spec)

ggplot(spct.df) +geom_col(aes(x=1/spct$freq,y=spec), width=2, position = "identity") +
  scale_x_continuous(breaks=seq(1,1200,50), minor_breaks=seq(1,1200,10)) +
  labs(x="DoY",title="Simulated data, 2017-2019") +
  theme +
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplot(spct.df) +geom_col(aes(x=1/spct$freq,y=spec), width=0.25) +
  scale_x_continuous(limits = c(0,70), breaks=seq(1,70,5), minor_breaks=seq(1,70,1)) +
  labs(x="DoY",title="Simulated data, 2017-2019", subtitle="Zoomed in") +
  theme

###
source("Modules/dbData.R")
df.br <- db.data(vw="vwOrdersByDay",db="BrazilianEcommerce")
str(df.br)

df.br$date <- as.Date(df.br$date)

start <- min(df.br$date)
end <- max(df.br$date)
count(df.br)
end-start

cal <- seq(as.Date(start), as.Date(end), 1) 
df.cal <- data.frame(x=1:length(cal), date=cal) 
str(df.cal)

df.br.full <- left_join(df.cal, df.br, by=c("date"="date")) %>%
  mutate(orders=ifelse(is.na(orders),0,orders),
         sales=ifelse(is.na(sales),0,sales),
         units=ifelse(is.na(units),0,units),
         dow=wday(date))

ggplot(df.br.full) +
  geom_line(aes(x=date,y=orders)) +
  scale_x_date(date_breaks="1 month", date_labels="%b %d, %y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Brazilian Ecommerce Data",
       x="Date", y="Orders")

# the spike
ggplot(df.br.full) +
  geom_line(aes(x=date,y=orders)) +
  scale_x_date(date_breaks="1 day", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2017-11-15"),as.Date("2017-12-13"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1), panel.grid.minor = element_blank()) +
  theme +
  labs(title="Brazilian Ecommerce Data", subtitle="Zoomed in")

# average price "black friday" and overall
df.tmp <- df.br.full[df.br.full$date=="2017-11-24",] 
p.bf <- mean(df.tmp$sales/df.tmp$units)
p.oa <- sum(df.br.full$sales)/sum(df.br.full$units)
(p.oa-p.bf)/p.oa *100

filter(df.br.full, date>"2017-01-01") %>% slice_head(n=10)
# cut-off > 2017-01-04
filter(df.br.full, date>"2018-08-20") %>% slice_head(n=10)
# cut-off < 2018-08-24
df.br.full <- filter(df.br.full, date>"2017-01-04") %>% filter(date<"2018-08-20")

# remove the spike
df.br.full[df.br.full$date=="2017-11-24" | 
             df.br.full$date=="2017-11-25", "orders"] <- median(df.br.full$orders)

ggplot(df.br.full) +
  geom_line(aes(x=date,y=orders)) +
  scale_x_date(date_breaks="1 month", date_labels="%b %d, %y") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Brazilian Ecommerce Data") +
  geom_smooth(aes(x=date,y=orders), method="loess", span=0.9) +
  geom_smooth(aes(x=date,y=orders), method="lm", colour="red")

ggplot(df.br.full) +
  geom_line(aes(x=date,y=orders)) +
  scale_x_date(date_breaks="1 month", date_minor_breaks="1 week", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2017-12-25"),as.Date("2018-08-26"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major.x = element_blank())+
  theme +
  labs(title="Brazilian Ecommerce Data", subtitle="Zoomed in",
       x="Date", y="Orders")

ggplot(df.br.full) +
  geom_line(aes(x=date,y=orders)) +
  scale_x_date(date_breaks="1 day", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2018-02-25"),as.Date("2018-03-26"))) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title="Brazilian Ecommerce Data", subtitle="Zoomed in")

# weekly pattern
p <- predict(loess(orders~x,df.br.full, span=0.9))

df.br.full$t <- p
df.br.full$odt <- df.br.full$orders -df.br.full$t

br.med <- median(df.br.full$orders)
br.sd.med <- sd(df.br.full$orders)/br.med

br.g.w <- df.br.full %>% group_by(dow) %>% 
  summarise(y=(sum(odt)/n())/br.med, n=n()) %>%
  mutate(y=y*100)

ggplot(br.g.w) +
  geom_col(aes(x=dow,y=y), position="identity", fill="steelblue", alpha=0.8) +
  scale_x_continuous(labels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
                     breaks = seq(1,7,1)) +
  labs(x="DoW",y="Orders deviation, % of median",
       title="Weekly pattern",
       caption="Red dotted lines 1 standard deviation of original data, % of median") +
  geom_hline(yintercept=br.sd.med*100, colour="red", linetype="dotted") +
  geom_hline(yintercept=-br.sd.med*100, colour="red", linetype="dotted") +
  theme

spct.br <- spectrum(df.br.full$orders, log="no")
spct.df.br <- data.frame(freq=spct.br$freq, spec=spct.br$spec)

ggplot(spct.df.br) +geom_col(aes(x=1/freq,y=spec), width=1) +
  scale_x_continuous(limits = c(0,300), breaks=seq(1,300,10), minor_breaks=seq(1,300,1)) +
  labs(x="DoY",title="Brazilian Ecommerce Data", subtitle="Zoomed in") +
  theme +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# acf
a <- acf(df.br.full$orders, lag.max = 300)
df.a <- data.frame(lag=a[["lag"]],acf=a[["acf"]])
ggplot(df.a) +
  geom_col(aes(x=lag,y=acf), width=0.25) +
  scale_x_continuous(breaks=seq(0,300,10)) +
  scale_y_continuous(breaks=seq(-0.5,1.1,0.1))

# let's decompose and isolate lower frequency patterns if any
ts <- msts(df.br.full$orders, seasonal.periods=c(7))
mstl.br <- mstl(ts) # this function requires 2 full periods +1 observation! 

autoplot(mstl.br)

mstl.br.df <- as.data.frame(mstl.br)
mstl.br.df$date <- df.br.full$date

spct.br2 <- spectrum(mstl.br.df$Trend, log="no")
spct.df.br2 <- data.frame(freq=spct.br2$freq, spec=spct.br2$spec)

ggplot(spct.df.br2) +geom_col(aes(x=1/freq,y=spec), width=0.5) +
  scale_x_continuous(limits = c(0,200), breaks=seq(0,200,10), minor_breaks=seq(1,200,1)) +
  labs(x="DoY",title="Brazilian Ecommerce Data") +
  theme +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# detrend >>
p <- predict(lm(Trend~x, mstl.br.df))
df.p <- data.frame(x=mstl.br.df$x,dt=mstl.br.df$Trend-p)
ggplot(df.p) +geom_line(aes(x=x,y=dt))

a <- acf(df.p$dt, lag.max=300)
# << detrend

df.a <- data.frame(lag=a[["lag"]],acf=a[["acf"]])
ggplot(df.a) +
  geom_col(aes(x=lag,y=acf), width=0.25) +
  scale_x_continuous(breaks=seq(0,300,10)) +
  scale_y_continuous(breaks=seq(-0.5,1.1,0.1))

a <- acf(mstl.br.df$Remainder, lag.max=300)
df.a <- data.frame(lag=a[["lag"]],acf=a[["acf"]])
ggplot(df.a) +
  geom_col(aes(x=lag,y=acf), width=0.25) +
  scale_x_continuous(breaks=seq(0,300,10)) +
  scale_y_continuous(breaks=seq(-0.5,1.1,0.1))

# plot remainder histogram with norm distribution
bins=30
remainder.l <- length(mstl.br.df$Remainder)
remainder.max <- max(mstl.br.df$Remainder)
remainder.min <- min(mstl.br.df$Remainder)
remainder.b <- ifelse(abs(remainder.min)>abs(remainder.max), abs(remainder.min), abs(remainder.max))

x <- seq(-remainder.b,remainder.b,length.out=remainder.l)
y <- dnorm(x, 0, sd(mstl.br.df$Remainder))
df.pn <- data.frame(x=x,y=y*remainder.l*(remainder.max -remainder.min)/bins)

ggplot(mstl.br.df) +
  geom_histogram(aes(Remainder), bins=bins) +
  geom_line(data=df.pn, aes(x=x,y=y), colour="blue")

# forecast
f <- forecast(mstl.br, h=62)

nms <- attr(mstl.br,"dimnames")[[2]]
seasons <- paste0(nms[grep("Seasonal", nms, fixed=TRUE)], collapse=",")

df.f <- as.data.frame(f)
df.f$date = seq(max(df.br.full$date),max(df.br.full$date)+count(df.f)$n-1,1)

ggplot(df.br.full) +geom_line(aes(x=date,y=orders)) +
  geom_line(data=df.f, aes(x=date,y=`Point Forecast`), colour="blue") +
  
  geom_line(data=df.f, aes(x=date,y=`Lo 95`), colour="steelblue") +
  geom_line(data=df.f, aes(x=date,y=`Hi 95`), colour="steelblue") +
  geom_ribbon(data=df.f, aes(x=date,ymin=`Lo 95`,ymax=`Hi 95`), alpha=0.3, fill="blue") +
  
  scale_x_date(date_breaks="1 month", 
               date_labels="%b %d, %a", 
               limits=c(as.Date("2018-01-01"),max(df.f$date))
  ) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme +
  labs(title=paste0("MSTL Prediction with ",seasons), 
       subtitle="Zoomed in",
       caption="With 95% confidence interval")

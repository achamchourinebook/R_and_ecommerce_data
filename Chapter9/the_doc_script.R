library(dplyr)
library(lubridate)
library(ggplot2)
library(cowplot)
source("../Modules/dbData.R")

data <- db.data(vw="vwProducts_cats", db="BrazilianEcommerce")
data$StartDate <- as.Date(data$StartDate)

# add the dependent variable (y)
data$y <- 0

q <- quantile(data$UnitsNorm, c(0.90))

data[data$UnitsNorm>=q,"y"] <- 1

df <- data
df <- df %>% 
  mutate(Season = factor(ifelse(month(StartDate) %in% c(12,1,2), "Winter",
                                ifelse(month(StartDate) %in% c(3,4,5),"Spring",
                                       ifelse(month(StartDate) %in% c(6,7,8),"Summer","Fall")))),
         Category = factor(Category)
  )

str(df)

df <- df[,!(names(df) %in% c("UnitsNorm","StartDate"))]

xtabs(~Category+y, data=df)
xtabs(~Season+y, data=df)

###
frml <- paste0(names(df)[!(names(df) %in% c("id", "y"))],collapse="+")  
frml <- as.formula(paste0("y~",frml))

# split to train and test part
df.train <- df[sample(nrow(df), size=ceiling(nrow(df) *1.0)),]
df.test <- left_join(df, df.train, by=c("id"="id")) %>%
  filter(is.na(y.y)) %>%
  select(!contains(".y")) %>%
  rename_with(~gsub(".x","",.x))

df.sample <- df.train[sample(nrow(df.train), size=ceiling(nrow(df.train) *1.0)),]

model <- glm(frml, data=df.sample, family=binomial(link="logit"))

(msum <- summary(model))

# model significance
with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

with(model, 1-deviance/null.deviance)

###
as.data.frame(coef(msum)) %>% filter(`Pr(>|z|)` < 0.05) %>% arrange(desc(Estimate))

qstep <- 0.05
pmax <- 100

df.t <- df.sample
nms <- names(df.t)[!(names(df.t) %in% c("id", "y", "Predicted"))]

means <- vector("list", length(nms))
for(i in 1:length(nms))
{
  n <- nms[i]
  if(is.numeric(df.t[,n]))
  {
    q <- quantile(df.t[,n], seq(qstep,1,qstep))
    q <- data.frame(x=q, n=as.integer(gsub("%", "", names(q), fixed=TRUE)))
    q <- group_by(q, x) %>% summarise(p=min(n)) %>% filter(p<=pmax)
    x <- q$x
    
    means[[i]] <- list(n, mean(df.t[,n]), x)
    
  } else
  {
    mn <- levels(df.t[,n]) %>% .[order(.)] %>% min(.)
    
    means[[i]] <- list(n, mn, factor(levels(df.t[,n])))
  }
}

l.variables <- vector("list", length(means))

for(i in 1:length(means))
{
  # new dataframe
  dfname <- means[[i]][[1]]
  ln <- length(means[[i]][[3]])
  df.tmp <- data.frame(name=rep(dfname, length.out=ln))
  
  for(j in 1:length(nms))
  {
    k <- nms[j]
    if(dfname==k)
      fval <- means[[i]][[3]]
    else
    {
      x <- sapply(means, function(x){print(x[[1]])==k})
      fval <- rep(means[x][[1]][[2]], length.out=ln)
    }
    df.tmp[k] <- fval
  }
  
  # predict
  df.tmp$Predicted <- predict(model, newdata=df.tmp, type="response")
  df.tmp$Predicted <- df.tmp$Predicted*100
  
  # save
  df.tmp <- select(df.tmp, contains(c("name","Predicted",dfname))) %>%
    rename(Name=name,Variable=dfname)
  
  l.variables[[i]] <- df.tmp
}

theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
  plot.subtitle = element_text(hjust = 0.5, size = 10),
  plot.caption = element_text(face = "italic"),
  axis.title = element_text(face = "italic", size = 9)
)

df.all <- data.frame(Name=character(), Predicted=numeric())
l.plots <- vector("list", length(l.variables))

for(i in 1:length(l.variables))
{
  df.tmp <- l.variables[[i]]
  nm <- df.tmp[1,"Name"] 
  
  df.all <- rbind(df.all, df.tmp[,c("Name","Predicted")])
  
  deltap <- max(df.tmp$Predicted)-min(df.tmp$Predicted)
  plimit <- max(df.tmp$Predicted)
  
  gg <- ggplot(df.tmp) +
    geom_line(aes(x=Variable, y=Predicted, group="dummy"), 
              colour="steelblue", 
              size=1) +
    scale_y_continuous(limits = c(0,plimit)) +
    labs(title=nm, y="Probability", x="") +
    theme +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  l.plots[[i]] <- list(deltap, gg)
}

df.var.grp <- group_by(df.all, Name) %>%
  summarise(Delta=max(Predicted)-min(Predicted), Maxp=max(Predicted)) %>%
  arrange(desc(Delta))

p.list <- l.plots[order(desc(sapply(l.plots, function(x){x[[1]]})))]

save_plot("plots.png", 
          plot_grid(plotlist=lapply(p.list, function(x){x[[2]]}), ncol=2,nrow=5),
          ncol=2, nrow=5)

###
# prediction
df.sample$Predicted <- predict(model, newdata=df.sample, type="response")


# ROC
step <- 0.05
cutoff <- 0

fpr <- vector("double",length = 1/step)
tpr <- vector("double",length = 1/step)
coff <- vector("double",length = 1/step)

df.t <- df.sample
for(i in 1:length(fpr))
{
  tp <- count(filter(df.t, Predicted>=cutoff & y==1))$n
  p <- count(filter(df.t, y==1))$n
  
  fp <- count(filter(df.t, Predicted>=cutoff & y==0))$n
  n <- count(filter(df.t, y==0))$n
  
  fpr[i] <- fp/n
  tpr[i] <- tp/p
  coff[i] <- cutoff
  
  cutoff <- cutoff +step
  print(cutoff)
}

df.roc <- data.frame(fpr=fpr,tpr=tpr,cutoff=coff)
df.null <- data.frame(x=seq(0,1,step),y=seq(0,1,step)) 
df.cut <- data.frame(fpr=fpr,tpr=tpr,cutoff=coff)

ggplot(df.roc) +geom_line(aes(x=fpr,y=tpr, colour=cutoff), size=2) +
  geom_line(data=df.null, aes(x=x,y=y), colour="blue") +
  scale_color_gradientn(colours=c("#ff0000","yellow","lightgreen", "darkgreen","blue"), 
                        values=scales::rescale(c(0,
                                                 0.05,
                                                 0.1,
                                                 0.2,
                                                 0.5))) +
  theme

fp.weight <- 0.5 # neutral 
false.count <- fpr*fp.weight +(1-tpr)*(1-fp.weight)

ggplot(df.roc) +geom_line(aes(x=cutoff,y=false.count), colour="steelblue", size=1.5) +
  scale_x_continuous(breaks=seq(0,1,0.05)) +
  theme +
  theme(axis.text.x=element_text(angle=45, hjust=1))

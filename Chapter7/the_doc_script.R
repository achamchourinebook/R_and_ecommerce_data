# install.packages("arules")

library(arules)
library(dplyr)
source("Modules/dbData.R")

data <- db.data(vw="vwMBA", db="AdventureWorks")
data.sample <- data

ll <- split(data.sample[,"product"], data.sample[,"tid"])

tr <- as(ll, "transactions")

# support
total <- summarise(data, trn = n_distinct(tid))$trn
support <- group_by(data, product) %>% summarise(support=n()/total) %>% 
  arrange(desc(support)) %>% select(product, support)
q <- quantile(support$support,seq(0,1,0.25))
q

rules <- apriori(tr, parameter = list(support=0.01, confidence=0.1, minlen=2))
rules

rules.df <- as(rules, "data.frame")
str(rules.df)

rules.df %>% 
  mutate(support=round(support,4), confidence=round(confidence,4), lift=round(lift,4)) %>%
  select(rules, support, confidence, lift) %>%
  slice_head(n=10) 

###
t <- table(data[,"product"], data[,"tid"])
t[1:2,1:5]

d.obj <- dist(t, method="binary")

hc <- hclust(d.obj)

###
install.packages("dendextend")
library(dendextend)

dend <- as.dendrogram(hc, hang=0.2)

len <- attr(d.obj, "Size")
leaves <- data.frame(label=rep("", len), height = rep(-1, len))

s <- list(dend)
s2 <- NULL
height= -1
j <- 1

while(1==1)
{
  l <- length(s)
  if(l==0) break
  
  o <- s[l]
  s <- s[1:l-1]
  
  l <- length(o)
  
  for(i in 1:l)
  {
    l2 <- length(s2)
    h <- s2[l2]
    s2 <- s2[1:l2-1]
    
    if(!is.null(attr(o[[i]],"leaf")) && attr(o[[i]],"leaf")) 
    {
      leaves[j,"label"] <- attr(o[[i]],"label")
      leaves[j,"height"] <- h 
      j <- j+1
    } else
    {
      s <- c(s,o[[i]])
      
      height <- attr(o[[i]], "height")
      
      s2 <- c(s2, rep(height,length(o[[i]])))
    }
  }
}

maxHeight <- 0.95
dend <- prune(dend, leaves[leaves$height>=maxHeight,"label"])

font.size <- data.frame(v1=c(0,q[2:(length(q)-1)]),
                        v2=q[2:length(q)], 
                        size=c(0.4,0.6,0.8,1.0)) 

df.tmp <- inner_join(data.frame(label=labels(dend)), 
                     support, 
                     by=c("label"="product")) %>%
  merge(font.size) %>%
  filter(v1<support & support<=v2)

# new sizes
dend2 <- dend %>% 
  set("labels_cex", df.tmp$size) %>%
  set("branches_lwd", 0.7)

gdend <- as.ggdend(dend2)
mx <- 1
mn <- 0
ln <- 10
ticks <- seq(mn,mx,(mx-mn)/ln)
tick.h <- 0.1
x.coord <- -0.5

dd <- data.frame(x=rep(-tick.h/2+x.coord,ln+1),
                 xend=rep(tick.h/2+x.coord,ln+1),
                 y=ticks,yend=ticks)

annotations <- annotate("text", 
                        x=tick.h+x.coord, y=mx-ticks, 
                        label=ticks, hjust=0.5, vjust=0, size=3, 
                        colour="blue", 
                        fontface="italic")

gg <- ggplot(gdend, horiz=TRUE) +
  theme_grey() +
  labs(y="Likelihood of finding products together") +
  theme(panel.grid=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        panel.background = element_blank()
  ) +
  
  geom_segment(aes(x=x.coord, xend=x.coord, y=mn, yend=mx), colour="blue") +
  geom_segment(data=dd, aes(x=x,xend=xend,y=y,yend=yend), colour="blue") +
  annotations +
  geom_hline(data=dd, aes(yintercept=y), colour="blue", size=0.5, linetype="dotted") +
  labs(caption = "Probability of finding products together") +
  theme(plot.caption = element_text(face = "italic", hjust=0.5, colour="blue"))

# add support legend
support <- c(0.01,0.05,0.10,0.15)
point.size <- par("ps")
sizes.cex <- font.size$size
convert.factor <- 0.35278
sizes <- sizes.cex*point.size*convert.factor
labels <- scales::percent(support)

ddd <- data.frame(w=c("Size","Size","Size","Size"),
                  s=support,
                  x=1:4,
                  y=1:4)
gg.t <- ggplot(ddd) +geom_text(aes(label=w,x=x,y=y,size=factor(s))) +
  scale_size_manual(values=sizes, labels=labels) +
  labs(size="Support") +
  theme(legend.key=element_rect(fill=NA),
        legend.background=element_blank(),
        legend.box.background=element_rect(colour="black"))

legend <- cowplot::get_legend(gg.t)

gglegend <- ggdraw() +draw_grob(legend)

vp <- viewport(y=0.3,x=0.8, width=0.2, height=0.3)
gg
print(gglegend, vp=vp)


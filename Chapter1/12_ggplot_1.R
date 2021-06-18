library(lubridate)
library(dplyr)
library(ggplot2)

# simulate sales data
x <- seq(1,365,1)

# 2018
y <- runif(365, min=90, max=110) +x*0.02 # << a positive trend
# high volume, the same as previous but short and with a steep slope
y[251:266] <- runif(16, min=112, max=119) +(0:15)*1.3
df18 <- data.frame(day=x, sales=y, year="2018", stringsAsFactors=FALSE)

# 2019
y <- runif(365, min=85, max=115) -x*0.01 # << a negative trend
# high volume
y[103:118] <- runif(16, min=117, max=125) +(0:15)*1.4
df19 <- data.frame(day=x, sales=y, year="2019", stringsAsFactors=FALSE)

df.sales <- rbind(df18, df19)
str(df.sales)

p <- ggplot(df.sales, aes(x=day,y=sales, colour=year)) +
  geom_point(size=0.8) +
  geom_line(size=0.7)
p

# prepare breaks and labels
eof <- c(31,28,31,30,31,30,31,31,30,31,30,31)
breaks <- cumsum(eof)
labels <- paste0(rep("\n",times=12,each=1),month.abb)
labels <- paste0(as.character(eof),labels)

# add monthly and daily breaks
p <- p +
  scale_x_continuous(
    breaks=breaks,
    labels=labels,
    minor_breaks = seq(1,365,1)
  )
p

# generate subtitle
years <- distinct(select(df.sales, year))[["year"]]
years <- years[order(years, decreasing=TRUE)]

subtitle = paste0(years,collapse="/")

# labels
p <- p +
  labs(
    x="Day of the Year", 
    y="Sales", 
    title = "Year Over Year", 
    subtitle = subtitle, 
    caption = "Simulated Sales Data",
    colour = "Year"
  )
p

p <- p +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(face = "italic", size = 9)
  )
p

mcolors <- c("tomato3", "steelblue", "green", "black")

p <- p +
  scale_color_manual(values = mcolors)
p

# annotations
# find the extremums and add their dates  
d.a <- df.sales %>% group_by(year) %>% summarise(msales = max(sales))
d.a <- df.sales[df.sales$sales %in% d.a$msales,]
d.a$date <- ymd(paste0(d.a$year, "-01-01")) +days(d.a$day)

# add labels
d.a$text <- format(d.a$date, "Sellout %b %Y")
# and colours
d.a$color <- mcolors[1:length(d.a$year)]

# move labels slightly off peaks for an aesthetic reason
d.a$day <- d.a$day +1
d.a$sales <- d.a$sales +1
str(d.a)

p +annotate("text", 
            x=d.a$day, y=d.a$sales, 
            label=d.a$text, hjust=0, vjust=0, size=3, 
            colour=d.a$color, 
            fontface="italic")


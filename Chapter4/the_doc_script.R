# install.packages("sf", dependencies = TRUE) # takes some time
# install.packages("rnaturalearth", dependencies = TRUE)

# could throw an error, restart RStudio before installing, if happened
# install.packages("rnaturalearthhires", 
#                  repos = "http://packages.ropensci.org", 
#                  type = "source")
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(viridis)
library(dplyr)

world <- ne_countries(scale = "medium", returnclass = "sf")

# the world
ggplot(world) +
  geom_sf()

# zoom in
ggplot(world) +
  geom_sf() +
  coord_sf(ylim=c(48,52),xlim=c(-130, -120))

# country names list
data.frame(world) %>% distinct(name) %>% arrange(name)
data.frame(world) %>% distinct(sovereignt) %>% arrange(sovereignt)

brazil.states <- ne_states(country="Brazil", returnclass = "sf")

ggplot(brazil.states) +
  geom_sf() +
  theme_bw()

us.states <- ne_states(country="United States of America", returnclass = "sf")

lower48 <- ggplot(us.states) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw()  

alaska <- ggplot(us.states) +
  geom_sf() +
  coord_sf(crs = st_crs(3467), 
           ylim=c(449981.1884,2676986.5642), 
           xlim=c(-2255938.4795,1646517.6368 ), datum = NA) +
  theme_bw()  

hawaii <- ggplot(us.states) +
  geom_sf() +
  coord_sf(ylim=c(18.90,22.24), xlim=c(-160.55,-154.80 ), datum = NA) +
  theme_bw()

lower48.l <- -133
lower48.b <- 21.5

lower48.w <- -66 -(-130.00) 
lower48.h <- 50.00 -23.00

alaska.w <- -129.99 -(-172.40) 
alaska.h <- 71.35 -51.35

hawaii.w <- -154.80 -(-160.55) 
hawaii.h <- 22.24 -18.90

# Alaska and Hawaii scaled arbitrarily
lower48 +
  annotation_custom(grob=ggplotGrob(alaska), 
                    xmin = lower48.l, xmax = lower48.l +alaska.w/2.00,
                    ymin = lower48.b, ymax = lower48.b +alaska.h/2.00) +
  annotation_custom(grob=ggplotGrob(hawaii),
                    xmin = lower48.l+alaska.w/2.00+0.25, 
                    xmax = lower48.l+alaska.w/2.00+0.25 +hawaii.w*2.00,
                    ymin = lower48.b, 
                    ymax = lower48.b +hawaii.h*2.00)

# layouts
can.states <- ne_states(country="Canada", returnclass = "sf")

canada <- ggplot(can.states, aes(fill=iso_3166_2)) +
  geom_sf() +
  coord_sf(crs = st_crs(3347), 
           ylim=c(1000486.0,5606232.2754), 
           xlim=c(2365553.3321, 9683442.0814)) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha=1) +
  theme_bw() +
  labs(fill="Province") +
  geom_rect(ymin=1200486.0,ymax=2506232.0, 
            xmin=7858442,xmax=8683442, 
            fill=NA, 
            colour="black", size=1.0) +
  guides(fill=guide_legend(ncol=2)) +
  theme(legend.key.size = unit(0.8,"line"))

can.maritime <- filter(can.states, 
                       iso_3166_2=="CA-PE" | 
                         iso_3166_2=="CA-NB" | 
                         iso_3166_2=="CA-NS" | 
                         iso_3166_2=="CA-NL" | 
                         iso_3166_2=="CA-QC")
maritime <- ggplot(can.maritime, aes(fill=iso_3166_2)) +
  geom_sf() +
  coord_sf(crs = st_crs(3347), 
           ylim=c(1200486.0,2506232.0), 
           xlim=c(7858442, 8683442)) +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha=1) +
  theme_bw() +
  labs(fill="Province") +
  
  theme(legend.key.size = unit(0.8,"line"))

# show them together
ggplot() +
  coord_fixed(xlim=c(0,3),ylim=c(0,1), expand=FALSE) +
  annotation_custom(ggplotGrob(canada), xmin=0, xmax=2.1, ymin=0, ymax=1) +
  annotation_custom(ggplotGrob(maritime), xmin=2.3, xmax=3, ymin=0, ymax=1) +
  theme_void()

# install.packages("cowplot", dependencies = TRUE)
library(cowplot)

plot_grid(canada, maritime,nrow=1,rel_widths=c(2.4,1))
plot_grid(plotlist=list(canada, maritime, canada, maritime),nrow=2,rel_widths=c(2.4,1))

# with data
source("Modules/dbData.R")

df.sales <- db.data(vw="vwSalesByState")
str(df.sales)

us.states$state = us.states$postal

us.states.sales <- left_join(us.states, df.sales, by=c("state"="StateProvinceCode"))

install.packages("scales")
library(scales)
# lower 48
lower48 <- ggplot(us.states.sales, aes(fill=sales)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_viridis(option = "D", discrete = FALSE, alpha=1, 
                     na.value="whitesmoke",
                     labels = scales::comma) +
  labs(fill="Sales")

alaska <- ggplot(us.states.sales, aes(fill=sales)) +
  geom_sf() +
  coord_sf(crs = st_crs(3467), 
           ylim=c(449981.1884,2676986.5642), 
           xlim=c(-2255938.4795,1646517.6368 ), 
           datum = NA) +
  theme_bw() +
  scale_fill_viridis(option = "D", discrete = FALSE, alpha=1, 
                     na.value="whitesmoke",
                     labels = scales::comma) +
  labs(fill="Sales") +
  theme(legend.position = "none")

hawaii <- ggplot(us.states.sales, aes(fill=sales)) +
  geom_sf() +
  coord_sf(ylim=c(18.90,22.24), xlim=c(-160.55,-154.80 ), datum = NA) +
  theme_bw() +
  scale_fill_viridis(option = "D", discrete = FALSE, alpha=1, 
                     na.value="whitesmoke",
                     labels = scales::comma) +
  labs(fill="Sales") +
  theme(legend.position = "none")

lower48 +
  annotation_custom(grob=ggplotGrob(alaska), 
                    xmin = lower48.l, xmax = lower48.l +alaska.w/2.00,
                    ymin = lower48.b, ymax = lower48.b +alaska.h/2.00) +
  annotation_custom(grob=ggplotGrob(hawaii),
                    xmin = lower48.l+alaska.w/2.00+0.25, 
                    xmax = lower48.l+alaska.w/2.00+0.25 +hawaii.w*2.00,
                    ymin = lower48.b, 
                    ymax = lower48.b +hawaii.h*2.00)

# year over year by state

df.sales.all <- db.data(vw="vwSalesByStateYear")
str(df.sales.all)
df.sales.Y2012 <- filter(df.sales.all, year==2012)
df.sales.Y2013 <- filter(df.sales.all, year==2013)
df.sales.yoy <- 
  left_join(df.sales.Y2013, df.sales.Y2012, 
            by=c("StateProvinceCode"="StateProvinceCode")) %>%
  mutate(yoy = (sales.x-sales.y)/sales.y*100)

arrange(df.sales.yoy, yoy) 

us.states.yoy <- left_join(us.states, df.sales.yoy, by=c("state"="StateProvinceCode"))

ggplot(us.states.yoy, aes(fill=yoy)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_gradient2(low="red", 
                       high="green",
                       mid="yellow",
                       midpoint=0,
                       na.value="whitesmoke") +
  labs(fill="YoY %") +
  ggtitle("Sales Year over Year, 2013/2012") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(us.states.yoy, aes(fill=yoy)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_gradientn(colours=c("#ff0000","#ff9600","grey","lightgreen","green","darkgreen"), 
                       values=scales::rescale(c(-100,
                                                -1,
                                                0,
                                                1,
                                                200,
                                                1750)),
                       na.value = "whitesmoke") +
  labs(fill="YoY %") +
  ggtitle("Sales Year over Year, 2013/2012") +
  theme(plot.title = element_text(hjust = 0.5))

# using multiple scales
library(ggnewscale)
us.states.yoy1 <- filter(us.states.yoy, yoy <= -0.01)
us.states.yoy2 <- filter(us.states.yoy, yoy > -0.01)
us.states.yoy3 <- filter(us.states.yoy, is.na(yoy))

ggplot(data=us.states.yoy) +
  geom_sf(data=us.states.yoy1,aes(fill=yoy)) +
  scale_fill_gradientn(colours=c("#ff0000","#ff9600"), 
                       values=scales::rescale(c(-200, -0.01)),
                       na.value = "whitesmoke") +
  labs(fill="") +
  theme(legend.key.size = unit(0.8,"line")) +
  
  new_scale_fill() +
  geom_sf(data=us.states.yoy2, aes(fill=yoy)) +
  scale_fill_gradientn(colours=c("yellow3","lightgreen","green","darkgreen"), 
                       values=scales::rescale(c(-0.01, 0.5, 200, 1750)),
                       na.value = "whitesmoke") +
  geom_sf(data=us.states.yoy3, fill="whitesmoke") +
  
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +  
  labs(fill="YoY %") +
  
  theme(legend.key.size = unit(0.8,"line")) +
  theme_bw() +
  ggtitle("Sales Year over Year, 2013/2012") +
  theme(plot.title = element_text(hjust = 0.5))

library(ggrepel)
# labels, state midpoint is $latitude, $longitude
labels <- select(data.frame(us.states.yoy3), state, latitude, longitude, yoy)

top2 <- data.frame(us.states.yoy2) %>% 
  arrange(desc(yoy)) %>% 
  slice(1:2) %>%
  select(state, latitude, longitude, yoy)

bot2 <- data.frame(us.states.yoy1) %>% 
  arrange(yoy) %>% 
  slice(1:2) %>%
  select(state, latitude, longitude, yoy)

labels <- rbind(labels, top2)
labels <- rbind(labels, bot2)
labels <- filter(labels, state != "HI" & state != "AK")

# note crs: it's important to have the same as in the main geo data set!
labels.sf <- st_as_sf(labels, coords = c("longitude","latitude"), crs="WGS84", remove=FALSE)

pp <- ggplot(data=us.states.yoy) +
  geom_sf(data=us.states.yoy1,aes(fill=yoy)) +
  scale_fill_gradientn(colours=c("#ff0000","#ff9600"), 
                       values=scales::rescale(c(-200, -0.01)),
                       na.value = "whitesmoke") +
  labs(fill="") +
  theme(legend.key.size = unit(0.8,"line")) +
  
  new_scale_fill() +
  geom_sf(data=us.states.yoy2, aes(fill=yoy)) +
  scale_fill_gradientn(colours=c("yellow3","lightgreen","green","darkgreen"), 
                       values=scales::rescale(c(-0.01, 0.5, 200, 1750)),
                       na.value = "whitesmoke") +
  geom_sf(data=us.states.yoy3, fill="whitesmoke") +
  
  geom_sf(data=labels.sf) +
  
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +  
  labs(fill="YoY %") +
  
  theme(legend.key.size = unit(0.8,"line")) +
  
  theme_bw() +
  ggtitle("Sales Year over Year, 2013/2012") +
  theme(plot.title = element_text(hjust = 0.5))

pp +
  geom_label_repel(data=labels.sf, 
                   x=labels.sf$longitude, 
                   y=labels.sf$latitude, 
                   label = labels.sf$state)

# as we want the same colour scale for all graphs, we will use limits, see below
sales.min <- min(df.sales.all$sales)
sales.max <- max(df.sales.all$sales)

years.df <- distinct(df.sales.all, year) %>% 
  filter(!is.na(year) & year != 2014) %>% arrange(year)
years <- years.df[,"year"]

years.l <- vector("list", length(years))

i <- 1
for(y in years)
{
  # join here to avoid appearing of "uncharted" territories
  sales.df <- left_join(us.states, 
                        filter(df.sales.all, year==y), 
                        by=c("state"="StateProvinceCode"))  
  
  y.df <- ggplot(sales.df, aes(fill=sales)) +
    geom_sf() +
    coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
    theme_bw() +
    scale_fill_viridis(limits = c(sales.min, sales.max),
                       option = "D", discrete = FALSE, alpha=1,
                       na.value="whitesmoke",
                       labels = scales::comma) +
    labs(fill="Sales") +
    ggtitle(y) +
    theme(plot.title = element_text(hjust = 0.5))
  
  years.l[[i]] <- y.df
  i <- i +1
}

plot_grid(plotlist=years.l, ncol=2, rel_widths = c(1,1))

# legend separately: horizontal
sales.df <- left_join(us.states, 
                      filter(df.sales.all, year==2010), 
                      by=c("state"="StateProvinceCode"))  

with.legend.h <- 
  ggplot(sales.df, aes(fill=sales)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_viridis(limits = c(sales.min, sales.max),
                     option = "D", discrete = FALSE, alpha=1,
                     na.value="whitesmoke",
                     labels = scales::comma) +
  ggtitle("2010") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = unit(2.0, "line")) +
  guides(fill = guide_colorbar(title = "Sales",
                               label.position = "bottom",
                               title.position = "left", 
                               title.vjust = 1,
                               barwidth = 20)) # default unit "line" 

with.legend.v <- 
  ggplot(sales.df, aes(fill=sales)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_viridis(limits = c(sales.min, sales.max),
                     option = "D", discrete = FALSE, alpha=1,
                     na.value="whitesmoke",
                     labels = scales::comma) +
  ggtitle("2010") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "left",
        legend.direction = "vertical"
  ) +
  labs(fill="Sales")

# extract the legend
legend.h <- cowplot::get_legend(with.legend.h)
legend.v <- cowplot::get_legend(with.legend.v)

# plot graphs with no legend
years.l <- vector("list", length(years))

i <- 1
for(y in years)
{
  sales.df <- left_join(us.states, 
                        filter(df.sales.all, year==y), 
                        by=c("state"="StateProvinceCode"))  
  
  y.df <- ggplot(sales.df, aes(fill=sales)) +
    geom_sf() +
    coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
    theme_bw() +
    scale_fill_viridis(limits = c(sales.min, sales.max),
                       option = "D", discrete = FALSE, alpha=1,
                       na.value="whitesmoke",
                       labels = scales::comma) +
    labs(fill="Sales") +
    ggtitle(y) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  
  years.l[[i]] <- y.df
  i <- i +1
}

# with horizontal, best with even number of plots
g1 <- plot_grid(plotlist=years.l, ncol=2, rel_widths = c(1,1))
plot_grid(g1, legend.h, ncol = 1, nrow = 2, rel_heights = c(4,1))

# for the sake of demonstration replacing the last year with the legend
years.l[[4]] <- legend.v

# with vertical, best with odd number of plots
plot_grid(plotlist=years.l, ncol=2, rel_widths = c(1,1))

# animation
install.packages("gganimate", dependencies = TRUE)
library(gganimate)

i <- 1
for(y in years)
{
  sales.df <- left_join(us.states, 
                        filter(df.sales.all, is.na(year) | year==y), 
                        by=c("state"="StateProvinceCode")) %>%
    mutate(year = ifelse(is.na(year), y, year))
  
  if(i==1)
    anime.df <- sales.df
  else
    anime.df <- rbind(anime.df, sales.df)
  
  i <- i+1
}

anime <-
  ggplot(anime.df, aes(fill=sales)) +
  geom_sf() +
  coord_sf(ylim=c(23,50), xlim=c(-130, -66)) +
  theme_bw() +
  scale_fill_viridis(limits = c(sales.min, sales.max),
                     option = "D", discrete = FALSE, alpha=1,
                     na.value="whitesmoke",
                     labels = scales::comma) +
  labs(fill="Sales") +
  
  ggtitle("Year: {closest_state}") + # < use this for transition_states  
  #  ggtitle("Year: {current_frame}") + # < use this for transition_manual  
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  #  transition_manual(frames=year)
  transition_states(year)

a <- animate(anime)
# a <- animate(anime, nframes=4, fps=1, duration = 20)

anim_save("animated.gif", animation = a)

brazil.states <- ne_states(country="Brazil", returnclass = "sf")
brazil.states$state = substring(brazil.states$iso_3166_2, 4)

df.brazil.prod <- db.data(vw="vwSalesByStateAndProduct",db="BrazilianEcommerce")
str(df.brazil.prod)

df.brazil.prod %>% 
  group_by(state=StateProvinceCode, product) %>%
  summarise(sales = sum(sales)) %>%
  arrange(state, desc(sales))

# select 2 top products by state
dfg2.brazil <- df.brazil.prod %>% 
  group_by(state=StateProvinceCode, product) %>%
  summarise(sales = sum(sales)) %>%
  arrange(state, desc(sales)) %>%
  slice_head(n=2) %>%
  arrange(state)

# select 1 top product
dfg.brazil <- dfg2.brazil %>% group_by(state) %>% 
  arrange(state, desc(sales)) %>% slice_head(n=1)

# calc the difference between 1st and 2nd products
dfg.dif.brazil <- dfg2.brazil %>% group_by(state) %>% 
  summarise(dif = (max(sales)-min(sales))/min(sales)) %>%
  mutate(level = ifelse(dif<=0.05, 1,
                        ifelse(dif<0.1, 2,
                               ifelse(dif<0.8, 3, 4))))

df.sales.brazil <- left_join(dfg.brazil, dfg.dif.brazil, by=c("state"="state"))
df.sales.brazil %>% print(n=50)

brazil.prod <- left_join(brazil.states, df.sales.brazil, by=c("state"="state"))

brazil.midpoints <- filter(data.frame(brazil.prod), !is.na(level)) %>%
  select(state, longitude, latitude, level)

brazil.midpoints.sf <- st_as_sf(brazil.midpoints, 
                                coords = c("longitude","latitude"), 
                                crs="WGS84", remove=FALSE)

ggplot(brazil.prod) +
  geom_sf(aes(fill=product)) +
  geom_sf(data=brazil.midpoints.sf, aes(size=factor(level)), colour="red", shape=17) +
  coord_sf(ylim=c(-33,5), xlim=c(-80, -25)) +
  theme_bw() +
  scale_fill_viridis(option = "D", discrete = TRUE, alpha=1,
                     na.value="whitesmoke") +
  scale_size_manual(values = c(1.5,3,4,5.5), 
                    labels = c("<5","5-10","10-80",">80")) +
  labs(fill="Product", size="% over second place") +
  ggtitle("Top products") +
  theme(plot.title = element_text(hjust = 0.5))

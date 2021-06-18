# dplyr and data frames

library(dplyr)

# simple pipe
"some string" %>% substr(1,4)

# you can use a placeholder
1 %>% substr("some string",.,.+4)

x <- c("id", "date", "revenue", "orders") %>% .[.!="date"]
str(x)

# working data frames
df1 <- data.frame(
  oid = c(20,70,40,10,60,40), 
  discount = c(100,200,300,400,90,50), 
  date = c(as.Date("2019-01-02"), 
           as.Date("2019-01-03"), 
           as.Date("2019-12-31"),
           as.Date("2018-12-31"),
           as.Date("2020-01-03"),
           as.Date("2019-12-31")))

df2 <- data.frame(
  oid = c(10,20,30,40,50,60,70,80), 
  revenue = c(9000,1000,2000,3000,4000,5000,6000, 0), 
  date = c(as.Date("2018-12-31"),
           as.Date("2019-01-02"), 
           as.Date("2019-06-02"), 
           as.Date("2019-12-31"), 
           as.Date("2020-01-01"),
           as.Date("2020-01-03"),
           as.Date("2019-01-03"),
           as.Date("2017-12-30")))

filter(df2, year(date)==2018 | year(date)==2019 )
inner_join(df1, df2, by=c("date"="date"), suffix=c(".df1",".df2"))
mutate(df1, discount = -discount, year = year(date), m = month(date))
arrange(df1, desc(year(date)),month(date))
select(df1, date, discount)

# you can "chain" the functions 
arrange(df1, desc(year(date)),month(date)) %>% select(date, discount)
# you can change the order of functions in your chain
select(df1, date, discount) %>% arrange(desc(year(date)),month(date))
# but not always
arrange(df1, oid) %>% select(date, discount) 
select(df1, date, discount) %>% arrange(oid)

# top without groups, note the resulting order 
top_n(df2, -3, revenue)
top_n(df2, 3, revenue)

# groups
group_by(df1, year = year(date))
group_by(df1, year = year(date)) %>% top_n(1, discount)

# with aggregation
group_by(df2, year = year(date)) %>%
summarise(revenue.sum = sum(revenue), 
          revenue.min=min(revenue), 
          revenue.max=max(revenue), 
          orders = n_distinct(oid), 
          cnt = n())
# conditional aggregation
group_by(df2, year = year(date)) %>%
  summarise(revenue.sum = sum(ifelse(year%%2==0,revenue,0)), 
            orders = n_distinct(oid), 
            cnt = n())

# top_n on groups with aggragations:
# 1 level
group_by(df2, year = year(date)) %>%
  summarise(revenue.sum = sum(revenue)) %>%
  top_n(1,revenue.sum)
# 2 levels aggregated dataset
group_by(df2, year = year(date), month = month(date)) %>%
  summarise(revenue.sum = sum(revenue))
# 2 levels aggregated dataset with top_n
group_by(df2, year = year(date), month = month(date)) %>%
  summarise(revenue.sum = sum(revenue)) %>%
  top_n(1,revenue.sum)

inner_join(df1, df2, by=c("date"="date"), suffix=c(".df1",".df2")) %>%
  filter(year(date)!=2017) %>%
  mutate(revenue = -discount+revenue, year = year(date), m = month(date)) %>%
  group_by(year,m) %>%
  summarise(revenue.sum = sum(revenue), orders = n_distinct(oid.df1), cnt = n()) %>%
  mutate(month = month.abb[m]) %>%
  arrange(desc(year),m) %>%
  select(year, month, revenue.sum, orders, cnt)




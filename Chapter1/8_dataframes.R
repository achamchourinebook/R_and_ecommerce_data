# Data frames

# with predefined columns
df <- data.frame(col1 = c(1,2,3), col2 = c(100,200,300), col3 = c("a","b","c"))
str(df)

# adding new columns
df <- data.frame(col1=c(1,2,3))
str(df)

df[["col2"]] = c(100,200,300)
df$col3 = c("a","b","c") # << df$col3 is a shortcut for df[["col3"]]
str(df)

# adding rows and combining data frames
dfr1 <- data.frame(id=c(1,2,3), revenue=c(100.00,200.00,300.00))

l <- list(4,400.00)
dfr <- rbind(dfr1, l)
str(dfr)

dfr2 <- data.frame(id=c(5,6,7), revenue=c(500,600,700))
dfr <- rbind(dfr, dfr2)
str(dfr)

# accessing data
df[,"col1"]  # << a column as a vector!
df[["col1"]] # << the same!
df$col1      # << the same

x1 <- df["col1"] # << # note the absence of secondary brackets or comma
str(x1) # << a new data frame with one column!

# all rows, a few columns
dfn <- df[,c("col1","col3")]
str(dfn)

# some rows, all columns
dfn <- df[df$col1==1 | df$col3 =="c",]
str(dfn)

# some rows, some columns
dfn <- df[df$col1==1 | df$col3 =="c",c("col1","col2")]
str(dfn)

# get it sorted
dfs <- df[order(df$col3, decreasing=TRUE),]
dfs

# data updates
# single data frame
df$col3 = as.character(df$col3)
str(df)

df[df$col1==2|df$col1==3,"col1"] <- c(10,30)
df

# update one data frame with data from the other
# by binding on column of unique values 
df1 <- data.frame(
  id = c(10,20,30), 
  revenue = c(100,200,300), 
  date = c(as.Date("2019-01-02"), 
           as.Date("2019-06-01"), 
           as.Date("2019-12-31")))

df2 <- data.frame(
  id = c(1,2,3,4,5), 
  revenue = c(1000,2000,3000,4000,5000), 
  date = c(as.Date("2019-01-02"), 
           as.Date("2019-06-02"), 
           as.Date("2019-12-31"), 
           as.Date("2020-01-01"),
           as.Date("2020-01-03")))
df1$date
df2$date

# check that these two returns the same number of rows
df2[df2$date %in% df1$date,]
df1[df1$date %in% df2$date,]
# so we can replace the values
df1[df1$date %in% df2$date,"revenue"] = df2[df2$date %in% df1$date,"revenue"]
df1

# add one more row with the same date to df1
df1 <- rbind(df1, list(40,400,as.Date("2019-12-31")))
df1$date

# now these two returns different number of rows
df2[df2$date %in% df1$date,]
df1[df1$date %in% df2$date,]
# so, the attempt to update will result in error
df1[df1$date %in% df2$date,"revenue"] = df2[df2$date %in% df1$date,"revenue"]

# this time no assumption is made about data
# reset the values
df1$revenue = c(100,200,300,400)

# this combination of all.x and all.y is an equivalent of the left join 
m <- merge(df1,df2,by.x="date",by.y="date",all.x=TRUE,all.y=FALSE,suffixes=c("",".y"))
# now we can update the revenue column
m$revenue <- ifelse(!is.na(m$revenue.y),m$revenue.y,m$revenue)
# an return columns we are interested in (if needed)
df1 <- m[,c("id","revenue","date")]
df1


# plots

library(ggplot2)

df <- data.frame(day=c(1,2,3,4,5),sales=c(100.00,180.00,200.00,240.00,220.00))

ggplot(df) # << not much happens

ggplot(df,aes(x=day, y=sales)) # << better, but where the data points?

ggplot(df,aes(x=day, y=sales)) +
  geom_point() +
  geom_line()

# show colours
colours()

ggplot(df,aes(x=day, y=sales)) +
  geom_point(colour="blue", size=3, shape=15) +
  geom_line(colour="#0000FF", size=1, linetype="dotted")

# more graphs on the same canvas
# add two more datasets
df2 <- data.frame(day=c(1,2,3,4,5),sales=c(110.00,190.00,210.00,250.00,230.00))
df3 <- data.frame(day=c(1,2,3,4,5),sales=c(80.00,160.00,180.00,220.00,200.00))

ggplot() +
  geom_point(data=df,aes(x=day, y=sales), colour="blue") +
  geom_line(data=df,aes(x=day, y=sales), colour="blue") +
  geom_point(data=df2,aes(x=day, y=sales), colour="red") +
  geom_line(data=df2, aes(x=day, y=sales), colour="red") +
  geom_point(data=df3,aes(x=day, y=sales), colour="green") +
  geom_line(data=df3, aes(x=day, y=sales), colour="green")
  
# check the colour, wth?
ggplot(df,aes(x=day, y=sales, colour="blue")) +
  geom_point()

ggplot(df,aes(x=day, y=sales, colour=sales)) +
  geom_point()

# note the difference the use of factor makes
ggplot(df,aes(x=day, y=sales, colour=factor(sales))) +
  geom_point()

ggplot(df,aes(x=day, y=sales, 
              colour=factor(sales), 
              shape=factor(sales), 
              size=factor(sales))) +
  geom_point()

# note how it separates groups now
ggplot(df,aes(x=day, y=sales, colour=factor(sales%/%200))) +
  geom_point() +
  geom_line()


# Date and time

library(lubridate)

# date
d <- ymd("2016-03-01")
d

# adding and subtracting days
d <- ymd("2016-03-01")
d -1
d -days(1) # << you can also use days() function

d <- ymd("2017-03-01")
d -1
d -days(1)

d <- ymd("2017-12-31") +1
d

# date parts
d <- ymd("2017-12-31")
day(d)
week(d)
month(d)
quarter(d, fiscal_start = 8) # << with year starting month
year(d)

wday(d) # << week day as number, by default week start on Sun
as.character(wday(d, label= TRUE, abbr = TRUE)) # << as name

qday(d) # << quarter day
yday(d) # << year day

# adding and subtracting months and years
d <- ymd("2015-10-31")
d %m+% months(1)  # << note the day
d +months(1)      # << note the result! 

d <- ymd("2015-12-31")
d %m+% months(2)  # << note the day
d +months(2)      # << note the result! 

d %m-% months(1)
d %m+% months(4)
d + months(4)     # << note the result!

d <- ymd("2016-02-29")
d %m-% years(1)
d - years(1)      # << note the result!
d %m+% years(1)

# date with the time part
dt <- ymd_hms("2016-03-01 22:01:54")
dt
second(dt)
minute(dt)
hour(dt)

dt +seconds(7)
dt +minutes(15)
dt -hours(23)

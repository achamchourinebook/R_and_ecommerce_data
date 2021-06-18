setwd("<your working directory>")
source("Modules/dbData.R")

d <- db.data(vw="vTimeSeries", db="AdventureWorks")
str(d)

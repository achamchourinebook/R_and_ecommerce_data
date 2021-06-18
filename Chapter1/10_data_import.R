# data import

# check the path
getwd()

# csv
f1 <- read.csv("Datasets/AW test2.csv", stringsAsFactors = FALSE)
str(f1)

# delim with "," as separator
f2 <- read.delim("Datasets/AW test2.csv", 
           header = TRUE, 
           sep = ",", 
           quote = "\"",
           dec = ".",
           stringsAsFactors = FALSE)
str(f2)

f2$date <- as.Date(f2$date)
str(f2)

# RODBC
server = "<your server>"
db = "<your database>"

conn <- RODBC::odbcDriverConnect(
  paste0('driver={SQL Server};',
         'server=',server,';',
         'database=',db,';',
         'trusted_connection=yes'))

sql = "exec <your stored procedure>"
df <- RODBC::sqlQuery(conn, sql, stringsAsFactors=FALSE)
str(df) # << this is a data frame!

RODBC::odbcCloseAll()

#install.packages("RODBC", dependencies = TRUE)
db.data <- function(
  server="<your server>", 
  db="AdventureWorks", 
  is.trusted=TRUE,
  sp=NA,
  vw=NA,
  user="RUser")
{
  # this function is a wrapper for db calls, so you don't have to remember
  # all the internals
  # function returns a data frame
  # sp: stored procedure name
  # vw: view name (or table you stored your prepared data in)
  # if both are provided (undesirable), the sp will be used
  # if is.trusted = FALSE the duncton will ask you for credentials
  
  tryCatch(
  {
    if(is.trusted)
    {
      conn <- RODBC::odbcDriverConnect(
        paste0('driver={SQL Server};',
               'server=',server,';',
               'database=',db,';',
               'trusted_connection=yes')) 
    } else
    {
      conn <- RODBC::odbcDriverConnect(
        paste0('driver={SQL Server};',
               'server=',server,';',
               'database=',db,';',
               'UID=', ifelse(is.na(user), rstudioapi::showPrompt(title="", message="User Name", default=""),user),';',
               'PWD=',rstudioapi::askForPassword("Password")))
    }
    
    if(!is.na(sp))
      sql <- paste0("EXEC ",sp)
    else if(!is.na(vw))
      sql <- paste0("select * from ",vw)
    else
      stop("Neither sp nor view is provided!")
      
    df <- RODBC::sqlQuery(conn, sql, stringsAsFactors=FALSE) 

    if(typeof(df) == "character")
    {
      # this is an error
      stop(df[1])
    }
    
    return(df)  
  }, error = function(e) {
    message(e)
  }, finally = {
    RODBC::odbcCloseAll()
  }
  )
}

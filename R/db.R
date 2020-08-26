# db connection
# conn <- boastUtils::db()
# https://db.rstudio.com/best-practices/managing-credentials/
#'@export
dbConnect <- function() {
  config <- boastUtils:::getConfig()
  
  conn <- Cushion$new(
    host = config$database$host,
    transport = config$database$transport,
    port = config$database$port,
    user = config$database$user,
    pwd = config$database$pwd
  )
  
  boastUtils:::initAppStorage(conn)
  
  return(conn)
}


# db create / base name off of current app id
initAppStorage <- function(conn) {
  id <- boastUtils:::getAppIdentifier()
  db_name <- NA_character_
  
  # Check if app is running locally or deployed on shinyapps.io
  if(is_local()) {
    
  } else {
    # use shinyapps.io id
  }
}

# db write

# db query

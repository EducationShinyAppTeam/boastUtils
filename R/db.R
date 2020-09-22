#' Connection to default Apache CoucheDB
#'
#' export
dbConnect <- function() {
  warning("dbConnect is in pre-release phase, use at your own risk.")
  
  config <- boastUtils:::.getConfig()
  
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

#' Determine which table/document to use based on environment
initAppStorage <- function(conn) {
  id <- boastUtils:::getAppIdentifier()
  db_name <- NA_character_
  
  # Check if app is running locally or deployed on shinyapps.io
  if(isLocal()) {
    
  } else {
    # use shinyapps.io id
  }
}

# db write

# db query

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
  if(is_local()) {
    # use uuid stored somewhere
    uuid <- uuid::UUIDgenerate()
    if(file.exists("DESCRIPTION")) {
      stored_uuid <- read.dcf("DESCRIPTION", fields = "UUID") 
      if(!is.na(stored_uuid[1])){
        
      } else {
        # store it
      }
    } else {
      
    }
    
  } else {
    # use shinyapps.io id
  }
}

# db write

# db query

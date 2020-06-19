# Setup Learning Locker configuration
.lockerConfig <- list(
  base_url = "https://learning-locker.stat.vmhost.psu.edu/",
  auth = "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk",
  agent = rlocker::createAgent()
)

# Return Learning Locker configuration
#'@export
getLockerConfig <- function() {
  return(.lockerConfig)
}

#' Learning Locker Statement Generation
#' todo: bind events to all fixed dashboard input items
bindInputEvents <- function(session) {
  observeEvent(session$input$tabs, {
    try({
      .storeStatement(session,
        .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", session$input$tabs, " tab."))
      )
    })
  }, ignoreInit = TRUE)
}

#' App cleanup functions
bindSessionEnd <- function(session) {
  onSessionEnded(function() {
    isolate({
      response <- httr::POST(
        url = "https://learning-locker.stat.vmhost.psu.edu/data/xAPI/statements", 
        config = add_headers(
          "Auth" = "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk",
          "Content-Type" = "application/json",
          "X-Experience-API-Version" = "1.0.1"
        ),
        body = .generateStatement(session, verb = "exited", description = "Session has ended."),
        encode = "json" 
      )
    })
  })
}

#' Create an xAPI Statement using rLocker
# todo: generalize to support most common use cases without reproducing all of rlocker::createStatement()
.generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA, extensions = NULL) {
  if (is.na(object)) {
    object <- paste0("#shiny-tab-", session$input$tabs)
  } else {
    object <- paste0("#", object)
  }
  
  stmt <- list(
    verb =  verb,
    object = list(
      id = paste0(boastUtils::getCurrentAddress(session), object),
      name = paste0(APP_TITLE),
      description = description
    )
  )
  
  if (!is.na(value)) {
    stmt$result <- list(
      response = paste(value)
    ) 
  }
  
  if (!is.null(extensions)) {
    stmt$result["extensions"] <- list(
      ref = extensions$ref,
      value = extensions$value
    )
  }
  
  statement <- rlocker::createStatement(stmt)
  
  return(statement)   
}

#' Store statement in configured locker
.storeStatement <- function(session, statement = NA) {
  
  response <- rlocker::store(session, statement)
  
  return(response)
}

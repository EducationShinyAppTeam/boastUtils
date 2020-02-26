#' Learning Locker Statement Generation
#' todo: bind events to all fixed dashboard input items
bindInputEvents <- function(session) {
  observeEvent(session$input$tabs, {
    try({
      .generateStatement(session, verb = "experienced", description = paste0("Navigated to ", session$input$tabs, " tab."))
    })
  }, ignoreInit = TRUE)
}

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
    stmt$result["extensions"] = list(
      ref = extensions$ref, value = extensions$value
    )
  }
  
  statement <- rlocker::createStatement(stmt)
  response <- rlocker::store(session, statement)
  
  return(response)   
}

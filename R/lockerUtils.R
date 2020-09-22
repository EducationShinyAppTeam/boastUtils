# Return auth based on current hosting environment
.getAuth <- function() {
  # equiv shiny:::inShinyServer
  auth <- NA_character_
  
  # Separate xAPI Statements in a different Store
  if(isLocal()) {
    auth <- "Basic NmIwZjAzMGE3ODJmYWY3ZGE0NDhjNGJkZjVkNTEwYTFkODQzNjM3NDoyNmU1MThjNTI2M2ZiZWEwMDIyM2YwYmIwZjM3OThmNTc2NWZiZGU5"
  } else {
    auth <- "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk"
  }
  
  return(auth)
}

.auth <- .getAuth()
.agent <- rlocker::createAgent()

# Setup Learning Locker configuration
.lockerConfig <- list(
  base_url = "https://learning-locker.stat.vmhost.psu.edu/",
  auth = .auth,
  agent = .agent
)

# Return Learning Locker configuration
#'@export
getLockerConfig <- function() {
  return(.lockerConfig)
}

#' Learning Locker Statement Generation
#' TODO: bind events to all fixed dashboard input items
.bindInputEvents <- function(session) {
  observe({
    
    # TODO: Make input list reactive and bind new observers only on creation
    inputs <- session$input
    
    sapply(isolate(names(inputs)), function(input) {
      observeEvent(inputs[[input]], {
        stmt <- NA
        
        if (input == "tabs" || input == "tab") {
          stmt <- generateStatement(
            session,
            verb = "experienced",
            object = paste0("shiny-tab-", inputs[[input]]),
            description = paste0("Navigated to ", inputs[[input]], " tab.")
          )
        } else {
          stmt <- generateStatement(
            session,
            verb = "interacted",
            object = input,
            description = paste0("Interacted with ", input, "."),
            response = inputs[[input]]
          )
        }
        try({
          storeStatement(session, stmt)
        })
      }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = -1)
    })
  })
}

#' App cleanup functions
.bindSessionEnd <- function(session) {
  onSessionEnded(function() {
    isolate({
      response <- httr::POST(
        url = "https://learning-locker.stat.vmhost.psu.edu/data/xAPI/statements", 
        config = add_headers(
          "Auth" = .lockerConfig$auth,
          "Content-Type" = "application/json",
          "X-Experience-API-Version" = "1.0.1"
        ),
        body = generateStatement(
          session,
          verb = "exited",
          description = "Session has ended."
        ),
        encode = "json" 
      )
    })
  })
}

#' Generate Statement
#' 
#' Create an xAPI Statement using rLocker
#' 
#' @examples 
#' generateStatement(
#'   session,
#'   verb = "answered",
#'   object = "SAMPLE_QUESTION_ID",
#'   description = "SAMPLE_QUESTION_OUTPUT",
#'   interactionType = "choice",
#'   response = "SAMPLE_RESPONSE",
#'   success = c(TRUE, FALSE),
#'   score = list(
#'     min = 0,
#'     max = 100,
#'     raw = 35,
#'     scaled = 0.35
#'   ),
#'   completion = FALSE
#' )
#' 
#' @seealso \link[rlocker]{getVerbList()}
#' @seealso \link[rlocker]{getInteractionTypes()}
#' 
#' @export
generateStatement <- function(
  session,
  verb = NA,
  object = NA,
  description = NA,
  interactionType = NA,
  response = NULL,
  success = NA,
  score = list(),
  completion = NA,
  extensions = NULL) {
  
  # Assumes input has corresponding DOM id to anchor to
  if (is.na(object)) {
    object <- paste0("#shiny-tab-", object)
  } else {
    object <- paste0("#", object)
  }
  
  stmt <- list(
    agent = .lockerConfig$agent,
    verb =  verb,
    object = list(
      id = paste0(boastUtils::getCurrentAddress(session), object),
      name = paste0(APP_TITLE),
      description = description
    ),
    result = list()
  )
  
  if (!is.null(response)) {
    stmt$result$response <- paste(response)
  }
  
  if (!is.na(interactionType)) {
    stmt$object$interactionType <- interactionType
  }
  
  if (length(score) > 0) {
    stmt$result$score <- score
  }
  
  if (!is.na(success)) {
    stmt$result$success <- success
  }
  
  if (!is.na(completion)) {
    stmt$result$completion <- completion
  }
  
  if (!is.null(extensions)) {
    stmt$result["extensions"] <- list(
      ref = extensions$ref,
      value = extensions$value
    )
  }
  
  # If result object is still empty remove it
  if (!length(stmt$result)) {
    stmt[["result"]] <- NULL
  }
  
  statement <- rlocker::createStatement(stmt)
  
  return(statement)   
}

#' Store xAPI Statement in configured Locker
#' @export
storeStatement <- function(session, statement = NA) {
  
  response <- rlocker::store(session, statement)
  
  return(response)
}

# WIP Not yet finalized
# TODO: Find if possible to get input type radio vs select vs text
.renderInputDebugger <- function(session) {
  
  input <- session$input
  output <- session$output
  
  Inputs <- reactive({
    inputs <- NULL
    values <- NULL
    
    for(i in seq_len(length(names(input)))){
      values <- paste(names(input)[i], input[[names(input)[i]]])
      inputs <- append(inputs, values)
    }
    inputs
  })
  
  output$inputDebugger <- renderTable({
    Inputs()
  })
}

#' .getAuth
#' 
#' Return auth based on current hosting environment
.getAuth <- function() {
  
  auth <- NA_character_
  
  # Separate local and deployed app xAPI Statements in different Stores.
  local <- isLocal()
  
  if(local) {
    auth <- "Basic NmIwZjAzMGE3ODJmYWY3ZGE0NDhjNGJkZjVkNTEwYTFkODQzNjM3NDoyNmU1MThjNTI2M2ZiZWEwMDIyM2YwYmIwZjM3OThmNTc2NWZiZGU5"
  } else {
    auth <- "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk"
  }
  
  return(auth)
}

#' getLockerConfig
#' 
#' Return Learning Locker configuration for current session.
#' 
#' @return 
#' ```
#' list(
#'   base_url,
#'   auth,
#'   agent = list(
#'     name,
#'     mbox,
#'     objectType
#'   ),
#'   language
#' )
#' ```
#' 
#' @seealso \link[rLocker]{get_locker_config()}
#' 
#' @export
getLockerConfig <- function() {
  return(getOption("locker_config"))
}

#' .logInteraction
#' 
#' Learning Locker Statement Generation
.logInteraction <- function(session, i) {
  stmt <- NA
  
  if (i == "tabs" || i == "tab" || i == "page" || i == "pages") {
    stmt <- generateStatement(
      session,
      verb = "experienced",
      object = paste0("shiny-tab-", session$input[[i]]),
      description = paste0("Navigated to ", session$input[[i]], " tab.")
    )
  } else {
    stmt <- generateStatement(
      session,
      verb = "interacted",
      object = i,
      description = paste0("Interacted with ", i, "."),
      response = session$input[[i]]
    )
  }
  try({
    storeStatement(session, stmt)
  })
}

#' .logSessionEnd
#' 
#' Try to log when a user ends their session
.logSessionEnd <- function(session) {
  response <- httr::POST(
    url = httr::build_url(
      httr::parse_url(
        paste0(getLockerConfig()$base_url, "/data/xAPI/statements")
      )
    ), 
    config = list(
      add_headers(
        "Auth" = getLockerConfig()$auth,
        "Content-Type" = "application/json",
        "X-Experience-API-Version" = "1.0.1"
      ),
      verbose = TRUE
    ),
    body = generateStatement(
      session,
      verb = "exited",
      description = "Session has ended."
    ),
    encode = "json" 
  )
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
#' @seealso \link[rLocker]{getVerbList()}
#' @seealso \link[rLocker]{getInteractionTypes()}
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
  extensions = list()) {

  tryCatch({
    # Assumes input has corresponding DOM id to anchor to
    if (is.na(object)) {
      # Search for current tab in `input$pages`.
      # May result in `shiny-tab-NA` if using unconventional naming / app structure. 
      page <- session$input$pages
      if(!is.null(page)){
        object <- tolower(page)
      } else if(is.null(page) & !is.null(session$input$tabs)) {
        message("Deprecation: dashboardSidebar(sidebarMenu(id)) should be updated to use `pages` if currently using the old naming convention `tabs`.")
        object <- tolower(session$input$tabs)  
      }
      object <- paste0("#shiny-tab-", object)
    } else {
      object <- paste0("#", object)
    }
    
    agent <- getLockerConfig()$agent
    name <- getAppTitle(short = TRUE, case = "snake")
      
    stmt <- list(
      agent = agent,
      verb =  verb,
      object = list(
        id = httr::build_url(
          httr::parse_url(
            paste0(boastUtils::getCurrentAddress(session), object)
          )
        ),
        name = name,
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
    
    if (length(extensions) > 0) {
      stmt$result$extensions <- extensions
    }
    
    # If result object is still empty remove it from the output.
    if (!length(stmt$result)) {
      stmt[["result"]] <- NULL
    }
    
    statement <- rLocker::createStatement(stmt)
    
    return(statement)
  }, error = function(e) {
    warning("Unable to create xAPI statement.", call. = FALSE)
  })  
  
}

#' storeStatement
#' 
#' Store xAPI Statement in configured Store.
#' 
#' @export
storeStatement <- function(session = NULL, statement = NA) {
  logging <- Sys.getenv("XAPI_LOGGING")
  
  if(logging) {
    tryCatch({
      if(!is.na(statement) & statement != ""){
        response <- rLocker::store(session, statement)
        return(response)
      } else {
        return(400)
      }
    }, error = function(e) {
      warning("Unable to store xAPI statement.", call. = FALSE)  
    })
  } else {
    message("xAPI Logging Disabled")
  }
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

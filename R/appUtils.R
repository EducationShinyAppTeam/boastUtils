# Create link to javascript files for package
.onAttach <- function(...) {
  shiny::addResourcePath("app", system.file("assets/js/", package = "boastUtils"))
}

#' Used to attach scripts to DOM in template.html head
#'@export
scripts <- function() {
  # htmlDependency js and css will be used in other functions with attachDependency
  dependencies <- list(
    htmltools::htmlDependency(
      name = "app",
      version = "1.0.0",
      src = c("href" = "app"),
      script = "app.js"
    )
  )
  htmltools::attachDependencies(htmltools::tags$head(), dependencies, append = TRUE)
}

# Injects config into the start of the server function
.injectBoastConfig <- function(server) {

  body <- as.list(body(server))
  
  call <- as.call(
    quote({
      connection <- boastUtils:::.boastInit(session)
    })
  )

  body(server) <- as.call(append(body, call, 1))

  return(server)
}

.boastInit <- function(session) {
  
  # Setup environment variables
  BASE_DIR <- normalizePath(getwd())
  APP_NAME <- basename(BASE_DIR)
  APP_ROOT <- BASE_DIR
  Sys.setenv("APP_NAME" = APP_NAME)
  Sys.setenv("APP_ROOT" = APP_ROOT)
  
  connection <- boastUtils:::.boastConnect(session)
  
  boastUtils:::.bindInputEvents(session)
  boastUtils:::.bindSessionEnd(session)
  
  return(connection)
}

.boastConnect <- function(session) {
  
  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, getLockerConfig())

  return(connection)
}

#' Gets page address from the current session
#'@export
getCurrentAddress <- function(session) {
  port <- ifelse(is.null(session$clientData$url_port), NULL, paste0(":", session$clientData$url_port))
  path <- ifelse(is.null(port), sub("/$", "", session$clientData$url_pathname), session$clientData$url_pathname)
  
  return(paste0(
    session$clientData$url_protocol, "//",
    session$clientData$url_hostname,
    port,
    path,
    session$clientData$url_search
  ))
}

#' Check to see if current environment is local or shinyapps.io
#' @export
isLocal <- function() {
  return(!nzchar(Sys.getenv("SHINY_PORT")))
}

#' Returns the working directory set at startup
#' @seealso Sys.getenv("PWD") on shinyapps.io
#' @export
getAppRoot <- function() {
  return(Sys.getenv("APP_ROOT"))
}

#' Gets app id based on current deployed state
#' @export
getAppIdentifier <- function() {
  id <- NA_character_
  
  # Generate a temp uuid
  temp_uuid <- uuid::UUIDgenerate()
  # If app is local, check if metadata file exists
  if(file.exists("DESCRIPTION")) {
    # Check to see if identifier is stored, if not, store it.
    DESCRIPTION <- file.path(getAppRoot(), "DESCRIPTION")
    stored_uuid <- read.dcf(DESCRIPTION, fields = "UUID")
    if (!is.na(stored_uuid[1])) {
      id <- stored_uuid
    } else {
      boastUtils:::setAppIdentifier(temp_uuid)
    }
  } else {
    # Suggest creating it / continue using temp id
    message(
      paste(
        "DESCRIPTION metadata file not file, consider creating one. See:",
        "  https://github.com/rstudio/shiny-examples/blob/master/001-hello/DESCRIPTION",
        sep = "\n"  
      )
    )
    id <- temp_uuid
  }
  
  return(id)
}

#' Gets properties set in boastUtils/config.yml
getConfig <- function() {
  install_path <- find.package("boastUtils")
  conf <- config::get(file = file.path(install_path, "config.yml"))
  return(conf)
}

#' Writes UUID to App DESCRIPTION file in project root
setAppIdentifier <- function(uuid) {
  UUID <- as.data.frame(uuid)
  success <- FALSE
  tryCatch({
    APP_ROOT <- getAppRoot()
    if (APP_ROOT != "") {
      DESCRIPTION <- file.path(APP_ROOT, "DESCRIPTION")
      write.dcf(UUID, file = DESCRIPTION, append = TRUE)
      success <- TRUE
    } else {
      warning("Unable to determine root directory, skipping DESCRIPTION file creation.")
    }
  }, error = function(err) {
    warning(err)
  })
  return(success)
}

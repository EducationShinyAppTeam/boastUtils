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

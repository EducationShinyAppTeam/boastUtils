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
injectBoastConfig <- function(server) {

  body <- as.list(body(server))
  call <- as.call(quote({connection <- boastUtils:::boastConnect(session)}))

  body(server) <- as.call(append(body, call, 1))

  return(server)
}

boastConnect <- function(session) {
  # Setup learning locker configuration
  config <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk",
    agent = rlocker::createAgent()
  )

  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, config)

  return(connection)
}

# Gets page address from the current session
#'@export
getCurrentAddress <- function(session) {
  return(paste0(
    session$clientData$url_protocol, "//",
    session$clientData$url_hostname,
    session$clientData$url_pathname, ":",
    session$clientData$url_port,
    session$clientData$url_search
  ))
}

#' @export
boastApp <- function(ui, server) {
  ui <- htmlTemplate(
    system.file("templates", "template.html", package = "boastUtils"),
    document_ = TRUE,
    body = ui
  )
  
  shinyApp(ui, injectBoastConfig(server))
}

injectBoastConfig <- function(server) {
  server_body <- body(server)
  last_call <- length(as.list(server_body))
  languageEl(body(server), which = last_call) <- str2lang("boastConnect(session)")

  return(server)
}

#' @export
boastConnect <- function(session) {
  # Setup locker configuration
  config <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic ZDQ2OTNhZWZhN2Q0ODRhYTU4OTFmOTlhNWE1YzBkMjQxMjFmMGZiZjo4N2IwYzc3Mjc1MzU3MWZkMzc1ZDliY2YzOTNjMGZiNzcxOThiYWU2",
    agent = rlocker::createAgent()
  )

  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, config)

  return(connection)
}

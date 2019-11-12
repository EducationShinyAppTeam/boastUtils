#' @export
boastApp <- function(ui, server) {
  ui <- htmlTemplate(
    system.file("templates", "template.html", package = "boastUtils"),
    document_ = TRUE,
    body = ui
  )

  shinyApp(ui, server)
}

#' @export
boastConfig <- function(session, config = NULL) {
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

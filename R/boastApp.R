#' BOAST App
#'
#' Overrides the default shinyApp ui and server config
#' - ui includes template for accessibility fixes and consistent styling
#' - server config adds setup defaults
#'
#' @export
boastApp <- function(ui, server, config = NA) {
  
  logging <- ifelse(!is.na(config), config$log, TRUE)
  
  Sys.setenv("XAPI_LOGGING" = logging)
  
  if(logging) {
    library(rlocker)
  }

  ui <- htmlTemplate(
    system.file("templates", "template.html", package = "boastUtils"),
    document_ = TRUE,
    body = ui
  )

  shinyApp(ui, .injectBoastConfig(server))
}

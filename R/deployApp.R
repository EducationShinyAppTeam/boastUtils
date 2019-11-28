#' todo:
#' - check if rsconnect account info is set if not ask for it
#' - write library manifest
#' - run lintr
#' - deployApp
#' @export
deployApp <- function(...) {
  rsconnect::appDependencies()
  packages <- list.files(path = "./packrat/desc")
  write(paste0("require(", packages, ")"), file = "dependencies.R")
  rsconnect::deployApp(lint = TRUE, ...)
}
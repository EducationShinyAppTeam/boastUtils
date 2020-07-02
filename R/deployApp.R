#' todo:
#' - check if rsconnect account info is set if not ask for it
#' - write library manifest
#' - run lintr
#' - deployApp
deployApp <- function(...) {
  
  continue <- TRUE
  
  checks <- list(
    # "packages" = checkPackages(),
    "code" = checkCode()
  )
  
  message("Checks passed: ")
  message(paste0(capture.output(checks), collapse = "\n"))
  # rsconnect::deployApp(lint = TRUE, ...)
  # Clean up workspace
  # remove(files)
}

continueChecks <- function(cont) {
  continue <- TRUE
  
  if (is.na(cont) || !cont) {
    continue <- FALSE
    message("Cancelling deployment.")
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  return(continue)
}

checkAccount <- function() {
  
}

checkPackages <- function() {
  status <- FALSE
  
  message("Creating package manifest in dependencies.R ...")
  rsconnect::appDependencies()
  packages <- list.files(path = "./packrat/desc")
  
  if (length(packages) > 0) {
    deps <- paste0("require(", packages, ")")
    write(deps, file = "dependencies.R")
    message(deps)
    status <- TRUE
  } else {
    cont <- askYesNo("No package dependencies found, are you sure this is intended?", default = FALSE)
    
    continueChecks(cont)
  }
  
  return(status)
}

checkCode <- function() {
  status <- FALSE
  
  message("Checking code")
  
  # todo: count warnings / errors ==> true if no errors
  lints <- lintr::lint_dir(
    path = getwd(),
    linters = with_defaults(
      boast_comment_linter,
      cyclocomp_linter,
      commented_code_linter,
      object_name_linter = NULL,
      camel_case_linter = NULL,
      trailing_blank_lines_linter = NULL,
      trailing_whitespace_linter = NULL
    )
  )
  
  counts <- list()
  
  for(i in 1:length(lints)) {
    type <- lints[[i]]$type
    counts[type] <- ifelse(is.null(counts[[type]]), 1, counts[[type]] + 1)
  }
  
  summary <- as.data.frame(counts)
  row.names(summary) <- ""
  
  message("Results: ")
  message(paste0(capture.output(summary), "\n"))
  
  if(!is.null(summary$errors)) {
    print(lints)
  } else {
    response <- askYesNo("Would you like to see suggested fixes?")
    
    if (response == TRUE) {
      print(lints)
    }
  }
  
  return(status)
}

#' .onAttach
#' 
#' Create link to html assets for package usage.
.onAttach <- function(...) {
  shiny::addResourcePath("app", system.file("assets/js/", package = "boastUtils"))
  shiny::addResourcePath("icons", system.file("assets/icons/", package = "boastUtils"))
}

#' scripts
#' 
#' Used to attach scripts to DOM in template.html head.
#' 
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

#' .injectBoastConfig
#' 
#' Injects boastApp configuration into the head of the default shinyApp server function; 
#' occurs **before** shinyApp::onStart(). This is important to setup environment data so
#' that it can be used within the default app.R/server.R server function. Expects shiny
#' session to be passed on runtime.
.injectBoastConfig <- function(server) {
  
  body <- as.list(body(server))
  
  # .boastInit is executed **after** shinyApp::onStart()
  call <- as.call(
    quote({
      connection <- boastUtils:::.boastInit(session)
    })
  )

  body(server) <- as.call(append(body, call, 1))
  
  return(server)
}

#' .boastInit
#' 
#' Initializes shinyApp state with BOAST specific utilities and
#' environment variables.
.boastInit <- function(session) {
  
  tryCatch({
    if (class(session)[1] != "ShinySession") {
      stop(call. = FALSE)
    } else {
      # Setup environment variables
      BASE_DIR <- normalizePath(getwd())
      APP_NAME <- basename(BASE_DIR)
      APP_ROOT <- BASE_DIR

      Sys.setenv("APP_NAME" = APP_NAME)
      Sys.setenv("APP_ROOT" = APP_ROOT)
      
      logging <- Sys.getenv("XAPI_LOGGING")
      
      connection <- list(
        status = 501
      )
        
      # Store connection details
      if(logging) {
        connection <- boastUtils:::.boastConnect(session)  
        boastUtils:::.bindInputEvents(session)
        boastUtils:::.bindSessionEnd(session)
      } else {
        message("xAPI Logging Disabled")
      }
      
      # Store app metadata globally for shared use
      APP_META <<- getAppMeta()
      
      observeEvent(session$input$eventListener, {
        if(session$input$eventListener == "diagnostics") {
          showModal(modalDialog(
            title = "App Diagnostics",
            renderDiagnostics(session),
            size = "l",
            footer = tagList(
              modalButton("Close")
            )
          ))
          updateTextInput(session, "eventListener", value = "")
        }
      })
      
      return(connection)
    }  
  }, error = function(e) {
    message(
      paste(
        "boastApp: ShinySession not set!\n",
        "Please include it in your app's server definition.\n",
        "Example: server <- function(input, output, session) { ... }"
      )
    )
  })
  
  # Internal Server Error (HTTP 500).
  return(list("status" = 500))
}

#' .boastConnect
#' 
#' Setup asynchronous connection to Learning Record Store (LRS).
.boastConnect <- function(session) {
  
  # Setup Learning Locker configuration
  .auth <- .getAuth()
  .agent <- rLocker::createAgent()
  
  .lockerConfig <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = .auth,
    agent = .agent
  )
  
  # Initialize Learning Locker connection
  connection <- rLocker::connect(session, .lockerConfig)

  return(connection)
}

#' .bindInputEvents
#' 
#' App input observers
.bindInputEvents <- function(session) {
  observe({
    # TODO: Make input list reactive and bind new observers only on creation
    sapply(isolate(names(session$input)), function(i) {
      observeEvent(session$input[[i]], {
        .logInteraction(session, i)
      }, ignoreNULL = TRUE, ignoreInit = TRUE, priority = -1)
    })
  })
}

#' .bindSessionEnd
#' 
#' App cleanup functions, does not trigger when running locally.
.bindSessionEnd <- function(session) {
  onSessionEnded(function() {
    isolate({
      # Ignore curl::curl_fetch_memory warnings caused by exiting too soon 
      suppressWarnings({
        .logSessionEnd(session)
      })
    })
  })
}

#' getCurrentAddress
#' 
#' Returns current page address in session$clientData as validated url.
#' 
#' @usage 
#' getCurrentAddress(session)
#' 
#' @examples
#' > getCurrentAddress(session)
#' [1] "https://psu-eberly.shinyapps.io/Sample_App/#shiny-tab-challenge"
#' 
#' @return url
#' 
#'@export
getCurrentAddress <- function(session) {
  port <- ifelse(is.null(session$clientData$url_port), NULL, paste0(":", session$clientData$url_port))
  path <- ifelse(is.null(port), sub("/$", "", session$clientData$url_pathname), session$clientData$url_pathname)
  
  url <- httr::build_url(
    httr::parse_url(
      paste0(
        session$clientData$url_protocol, "//",
        session$clientData$url_hostname,
        port,
        path,
        session$clientData$url_search
      )
    )
  )
  
  return(url)
}

#' isLocal
#' 
#' Check to see if current environment is local or shinyapps.io.
#' Returns `TRUE` if local; `FALSE` if shinyapps.io.
#' 
#' @usage isLocal()
#' 
#' @return boolean
#' 
#' @export
isLocal <- function() {
  status <- !nzchar(Sys.getenv("SHINY_PORT"))
  return(status)
}

#' getAppRoot
#' 
#' Returns the working directory set at startup.
#' 
#' Default deployed path is `/srv/connect/apps/<DIRECTORY_NAME>`.
#' 
#' @usage getAppRoot()
#' 
#' @examples
#' > getAppRoot()
#' [1] "/srv/connect/apps/Sample_App"
#' 
#' @seealso Sys.getenv("PWD") on shinyapps.io
#' @return path
#' @export
getAppRoot <- function() {
  return(Sys.getenv("APP_ROOT"))
}

#' Retrieve App Identifier
#' 
#' Gets app id based on current deployed state
#' 
#' @export
getAppIdentifier <- function() {
  DESCRIPTION <- file.path(getAppRoot(), "DESCRIPTION")
  id <- NA_character_
  
  # Generate a temp uuid
  temp_uuid <- uuid::UUIDgenerate()
  
  # Check if metadata file exists
  if(file.exists(DESCRIPTION)) {
    # Check to see if identifier is stored, if not, store it.
    stored_uuid <- read.dcf(DESCRIPTION, fields = "UUID")
    if (!is.na(stored_uuid[1])) {
      id <- stored_uuid
    } else {
      boastUtils:::.setAppIdentifier(temp_uuid)
    }
  } else {
    # Suggest creating it / continue using temp id
    message(
      paste(
        "DESCRIPTION metadata file not found, consider creating one. See:",
        "  https://github.com/rstudio/shiny-examples/blob/master/001-hello/DESCRIPTION",
        sep = "\n"  
      )
    )
    id <- temp_uuid
  }
  
  return(id)
}

#' Retrive config file
#' 
#' Gets properties set in boastUtils/config.yml
.getConfig <- function() {
  install_path <- find.package("boastUtils")
  conf <- config::get(file = file.path(install_path, "config.yml"))
  return(conf)
}

#' Store app identifier
#' 
#' Writes Universally Unique ID (UUID) to App DESCRIPTION file in project root.
#' 
#' Assumes property does not exist in file already. 
.setAppIdentifier <- function(uuid) {
  UUID <- as.data.frame(uuid)
  success <- FALSE
  try({
    APP_ROOT <- getAppRoot()
    if (APP_ROOT != "") {
      DESCRIPTION <- file.path(APP_ROOT, "DESCRIPTION")
      write.dcf(UUID, file = DESCRIPTION, append = TRUE)
      success <- TRUE
    } else {
      warning("Unable to determine root directory, skipping DESCRIPTION file creation.")
    }
  })
  return(success)
}

#' Get metadata
#' 
#' Returns metadata stored in app's DESCRIPTION file.
#' 
#' @return list
#' 
#' @export
getAppMeta <- function() {
  meta <- NA_character_
  tryCatch({
    # Get root directory of current app.
    APP_ROOT <- getAppRoot()
    
    if (APP_ROOT != "") {
      # Set expected file path based on APP_ROOT directory.
      DESCRIPTION <- file.path(APP_ROOT, "DESCRIPTION")
      
      # Check if DESCRIPTION file exists.
      if(file.exists(DESCRIPTION)) {
        # Read contents of DESCRIPTION file; empty file returns a <0 x 0 matrix>.
        # If this causes problems try removing textConnection portion.
        con <- textConnection(shiny:::readUTF8(DESCRIPTION))
               on.exit(close(con), add = TRUE)
        contents <- read.dcf(con)
        
        # If DESCRIPTION file has content, return it as a list.
        if(length(contents)) {
          cols <- colnames(contents)
          vectors_allowed <- c("Authors@R", "LearningObjectives")
          meta <- lapply(cols, function(key) {
            value <- contents[1,key][[1]]
            if(key %in% vectors_allowed) {
              tryCatch({
                # Some keys can have multiple values stored in a vector.
                # Try to extract the quoted values; if unsuccessful, return unparsed value.
                value <- eval(parse(text = value))
              }, error = function(cond) {
                print(cond)
                return(NA)
              }, warning = function(cond) {
                print(cond)
                return(NULL)
              })  
            }
            return(value)
          })
          names(meta) <- cols
        }  
      } else {
        warning(
          paste("DESCRIPTION metadata file does not exist, please create one in your project root. Refer to:",
                "\n  https://educationshinyappteam.github.io/Style_Guide/coding.html#metadata")
        )
      }
    } else {
      warning("Unable to determine root directory, skipping DESCRIPTION.")
    }
  }, error = function(cond) {
    return(NA)
  })
  
  return(meta)
}

#' Get app title
#' 
#' Returns tile stored in app's DESCRIPTION file.
#' 
#' @usage getAppTitle()
#' 
#' @examples
#' > getAppTitle()
#' [1] "Sample App"
#' > getAppTitle(short = FALSE, case = "snake")
#' [1] "Sample_App__A_Lengthy_Title"
#' @return character
#' 
#' @export
getAppTitle <- function(case = "title", short = TRUE) {
  title <- NA_character_
  
  # Check if APP_META is set; retrieve if not.
  meta <- NA
  if(!exists("APP_META")) {
    meta <- getAppMeta()
  } else {
    meta <- APP_META
  }
  
  # Check if metadata list contains data and is not all NA's.
  # If checking lists for NA you'll end up with:
  #   the condition has length > 1 and only the first element will be used
  if(!all(is.na(meta))) {
    title <- ifelse(short, meta$ShortName, meta$Title)
    if(!is.na(title)) {
      # We are just going to assume it is in title case already (because it should be).
      if(case == "snake") {
        title <- gsub("[[:punct:]]", "", title) # Remove punctuation
        title <- gsub(" ", "_", title) # Replace spaces with underscores
        # TODO: stringr/stringi str_to_title would be much better for this
      }
    } else {
      value <- ifelse(short, "ShortName", "Title")
      warning(paste0("Specified value `", value, "` was not found in the metadata."))
    }
  } else {
    warning("Could not obtain metadata info.")
  }
  
  # @deprecated warning
  # if(exists("APP_TITLE") || exists("APP_DESCP")) {
  #   message(
  #     paste("Deprecation: Please move APP_TITLE and/or APP_DESCP to metadata file. Refer to:",
  #           "\n  https://educationshinyappteam.github.io/Style_Guide/coding.html#metadata")
  #   )
  # }
  
  return(title)
}

#' typesetMath
#'
#' The Server component for retriggering MathJax's typesetting which is especially
#' useful when you are updating inputs with mathematical expressions for quizzes
#' and games.
#'
#' @param session Required--the shiny session for each instance
#' @return Typeset LaTeX on page.
#' 
#' @examples
#' typesetMath(session)
#'
#' @export
typesetMath <- function(session) {
  session$sendCustomMessage('typeset-mathjax', NA)
}

renderDiagnostics <- function(session) {
  tagList(
    tags$details(tags$summary("clientData"), renderPrint({ reactiveValuesToList(session$clientData) })),
    tags$details(tags$summary("env"), renderPrint({ Sys.getenv() })),
    tags$details(tags$summary("input"), renderPrint({ reactiveValuesToList(session$input) })),
    tags$details(tags$summary("memory"), renderPrint({ gc() })),
    tags$details(tags$summary("sessionInfo"), renderPrint({ sessionInfo() }))
  )
}

#' citeApp
#'
#' A function which will generate how an app should be cited. This function
#' requires the presence of a DESCRIPTION file in the app's repository to run.
#' 
#' @return Character string of the app's citation
#'
#' @examples
#' citeApp()
#' 
#' [1] "Carey, R., and Hatfield, N. J. (2021). BOAST Utilities. [R Shiny app]. Available https://github.com/EducationShinyAppTeam/boastUtils"
#'
#' @export
citeApp <- function() {
  metaData <- getAppMeta()
  
  tryCatch({
    if(length(metaData) >= 1 && !is.na(metaData)) {
      authors <- "<<MISSING_AUTHORS@R>>"
      if(!is.null(metaData$`Authors@R`)) {
        autDF <- data.frame(
          family = unlist(metaData$`Authors@R`$family),
          given = unlist(metaData$`Authors@R`$given)
        )
        
        for (i in 1:nrow(autDF)) {
          autDF$role[i] <- paste(metaData$`Authors@R`[i]$role, collapse = ",")
        }
        
        # Remove contributors if they exist in meta
        ctb <- which(autDF$role == "ctb")
        if(length(ctb)) {
          autDF <- autDF[-ctb,]   
        }
        
        autDF$given <- sapply(
          X = autDF$given,
          FUN = function(x) {
            if (x == "Dennis") {
              fI <- "D. K."
            } else if (x == "Neil") {
              fI <- "N. J."
            } else {
              fI <- paste0(substr(x, start = 1, stop = 1), ".")
            }
            return(fI)
          }
        )
        
        autDF$name <- sapply(
          X = 1:nrow(autDF),
          FUN = function(x) {
            paste(autDF$family[x], autDF$given[x], sep = ", ")
          }
        )
        
        authors <- autDF
      }
      
      year <- ifelse(!is.null(metaData$Date), substr(metaData$Date, 1, 4), "<<MISSING_DATE>>")
      title <- ifelse(!is.null(metaData$Title), metaData$Title, "<<MISSING_TITLE>>")
      url <- ifelse(!is.null(metaData$URL), metaData$URL, "<<MISSING_URL>>")  
      
      listing <- paste0(
        ifelse(nrow(authors) > 1, {
          paste0(
            paste(authors$name[1:(nrow(authors) - 1)], collapse = ", "),
            ", and ", authors$name[nrow(authors)]
          )
        }, authors$name),
        paste0(" (", year, "). "),
        paste0(title, ". "),
        "[R Shiny app]. Available ",
        url
      )
      
      if(grepl("<<MISSING_", listing)) {
        warning(paste("Missing metadata detected please see: https://github.com/EducationShinyAppTeam/App_Template/blob/master/DESCRIPTION\n ",listing))   
      }
      
      return(listing)
    } else {
      warning("Unable to cite app, please check warnings.")
    }
  }, warning = function(cond) {
    warning(cond)
  })
}

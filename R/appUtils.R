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
  .agent <- rlocker::createAgent()
  
  .lockerConfig <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = .auth,
    agent = .agent
  )
  
  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, .lockerConfig)

  return(connection)
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
          meta <- lapply(as.data.frame(contents), function(value) {
            tryCatch({
              # Some keys can have multiple values stored in a vector.
              # Try to extract the quoted values; if unsuccessful, return unparsed value.
              return(eval(parse(text = value)))
            }, error = function(e) {
              return(value)
            })
          })
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
  
  # Check if APP_META is set.
  if(!is.null(dim(APP_META))) {
    title <- ifelse(short, APP_META$ShortName, APP_META$Title)
    if(!is.null(title)) {
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
  }
  
  # @deprecated warning
  if(exists(APP_TITLE) || exists(APP_DESCP)) {
    message(
      paste("Deprecation: Please move APP_TITLE and/or APP_DESCP to metadata file. Refer to:",
            "\n  https://educationshinyappteam.github.io/Style_Guide/coding.html#metadata")
    )
  }
  
  return(title)
}

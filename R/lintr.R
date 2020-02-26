#'linters
#'
#' Contains a set linters for code deployment and review.
#' @name linters
NULL

#' Comment Linter
#' 
#' As apposed to the built-in commented_code_linter,
#' this linter is intended to encourage the writing of better comments.
#' 
#' @todo: capture function definitions to see if previous line has any comments
#' 
#' @describeIn linters checks that code is commented
#' @export
boast_comment_linter <- function(source_file, recommended_length = 15) {
  lapply(ids_with_token(source_file, "COMMENT"),
    function(id) {
      parsed <- source_file$parsed_content[id, ]
      
      # Remove #'/# from start/end of strings so that it does not impact counts
      sanitized <- gsub("(^#[#\'\t\r\n\ ]*)|(\ #*$)", "", parsed$text)
      
      char_len <- nchar(sanitized)
      if(char_len < recommended_length) {
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "BOAST: This comment is a little short.",
          line = source_file$lines[parsed$line1],
          linter = "boast_comment_linter"
        )
      }
    }
  )
}

#' Function Linter
#' 
#' @describeIn linters checks that code is using function calls
#' @export
boast_function_linter <- function(source_file) {
  lapply(ids_with_token(source_file, "COMMENT"),
     function(id) {
         
     }
  )
}

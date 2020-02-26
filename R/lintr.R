#'linters
#'
#' Contains a set linters for code deployment and review.
#' @name linters
NULL

#' @describeIn linters checks that code is commented
#' @export
boast_comment_linter <- function(source_file) {
  
  # todo: Goals capture function definitions to see if previous line has any comments
  # todo: Check comment length
  
  lapply(ids_with_token(source_file, c("COMMENT", "FUNCTION")),
     function(id) {
       parsed <- source_file$parsed_content[id, ]
       
       # todo: Remove #'/# from string so that it does not impact counts
       sanitized <- parsed$text
       
       charlen <- nchar(sanitized)
       if(charlen < 19) {
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
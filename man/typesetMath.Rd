% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appUtils.R
\name{typesetMath}
\alias{typesetMath}
\title{typesetMath}
\usage{
typesetMath(session)
}
\arguments{
\item{session}{Required--the shiny session for each instance}
}
\value{
Typeset LaTeX on page.
}
\description{
The Server component for retriggering MathJax's typesetting which is especially
useful when you are updating inputs with mathematical expressions for quizzes
and games.
}
\examples{
typesetMath(session)

}

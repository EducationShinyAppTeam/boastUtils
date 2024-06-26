% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appUtils.R
\name{getAppTitle}
\alias{getAppTitle}
\title{Get app title}
\usage{
getAppTitle()
}
\value{
character
}
\description{
Returns title stored in app's DESCRIPTION file.
}
\examples{
> getAppTitle()
[1] "Sample App"
> getAppTitle(short = FALSE, case = "snake")
[1] "Sample_App__A_Lengthy_Title"
}

BOAST Utilities
================
EducationShinyAppTeam
November 13, 2019

<!-- README.md is generated from README.Rmd. Please edit that file -->

Used to provide reusable
[components](https://shiny.rstudio.com/articles/html-tags.html),
[gadgets](https://shiny.rstudio.com/articles/gadgets.html), and
[modules](https://shiny.rstudio.com/articles/modules.html) for the
[BOAST](https://github.com/EducationShinyAppTeam/BOAST) project.

## Installation

You can install this version of boastUtils from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("EducationShinyAppTeam/boastUtils")
```

## Creating an App

One way to accomplish serving a more standardized codebase was to
provide a wrapper for the default
[shinyApp](https://shiny.rstudio.com/reference/shiny/latest/shinyApp.html)
function. Instead of including the `shiny` package, include `boastUtils`
and write your app as normal. Be sure to replace `shinyApp` with
`boastApp`.

##### Example app.R

``` r
library(boastUtils)

ui <- fluidPage(
  # ... ui definitions ...
)

server <- function(input, output, session){
  # ... server logic ...
}

boastApp(ui = ui, server = server)
```

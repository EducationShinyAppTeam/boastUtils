library(shiny)
library(shinydashboard)

#' Comment

#' Define UI for App
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "APP_TITLE",
      tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/', icon("home"))),
      tags$li(class = "dropdown", tags$a(href='https://github.com/EducationShinyAppTeam/BOAST', icon("github"))),
      tags$li(class = "dropdown", actionLink("info", icon("info")))
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")), # testing
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "Challenge", icon = icon("gears"))
      ),
      tags$div(
        class = "sidebar-logo",
        psu_eberly_logo("reversed")
      )
    ),
    dashboardBody(
      tabItems(
        Prerequisites,
        Overview,
        Explore,
        Challenge
      )
    )
  )
)

#' Define server logic
server <- function(input, output, session) {
  cba <- "321"
  # test <- 1
  # tokens <- tokens[are_todo, ]
  abc <- "123"
}

#' Create Shiny App
shinyApp(ui = ui, server = server)

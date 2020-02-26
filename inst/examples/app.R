library(boastUtils)

# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "APP_TITLE",
    tags$li(class = "dropdown", tags$a(href = "https://shinyapps.science.psu.edu/", icon("home")))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Sample", tabName = "Sample", icon = icon("book"))
    ),
    tags$div(
      class = "sidebar-logo",
      psu_eberly_logo("reversed")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Sample",
        h2("Lorem Ipsum"),
        p("Donec suscipit scelerisque rutrum eros in nam leo purus id,
           dui senectus tellus lacus consequat dictumst condimentum himenaeos pellentesque,
           turpis viverra eu adipiscing etiam aenean risus vel.
           Augue etiam urna lectus erat metus amet adipiscing convallis penatibus vehicula consectetur,
           nisl curabitur vestibulum cubilia condimentum iaculis nunc dis gravida.
           Et diam ac per fames curae conubia himenaeos netus,
           facilisi sapien pretium parturient venenatis enim cursus dui,
           vitae litora sociosqu neque felis rutrum consequat.
           Metus vehicula ipsum enim eleifend risus montes libero, ac primis nam mus dui nibh,
           phasellus senectus varius nec gravida facilisi. Quisque taciti aliquam non ultricies convallis maximus,
           rutrum tempus euismod tristique sodales consectetur condimentum, vulputate egestas sit suspendisse porttitor.
           Torquent lobortis sociosqu accumsan condimentum ac mauris rhoncus lacinia,
           proin lacus leo scelerisque amet tempus purus vehicula, tempor aliquam primis placerat venenatis risus sed.
           Fames litora commodo turpis sit efficitur nisl ad curabitur malesuada,
           torquent nascetur lobortis natoque enim consequat nisi.")
      )
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  message("\nrlocker status")
  message_for_status(connection$status)
}

# Run the application
boastApp(ui = ui, server = server)

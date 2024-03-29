library(boastUtils)

# Define UI for application
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Sample App",
    tags$li(class = "dropdown", surveyLink()),
    tags$li(class = "dropdown", tags$a(href = "https://shinyapps.science.psu.edu/", icon("home")))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "pages",
      menuItem("Sample", tabName = "Sample", icon = icon("book"))
    ),
    tableOutput("inputDebugger"),
    tags$div(
      class = "sidebar-logo",
      sidebarFooter()
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Sample",
        h2("Lorem Ipsum"),
        renderIcon("valid"),
        tags$script('$(document).ready(function(){ $("input").on("focus", function(e){ Shiny.setInputValue("focusedElement", e.target.id);}); }); '),
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
           torquent nascetur lobortis natoque enim consequat nisi."),
        textInput("sample_input", label = "Sample Input", value = "Placeholder"),
        p(
          textOutput("output1")
        ),
        br(),
        h2("Icons"),
        p(
          uiOutput("sampleIconCorrect", inline = TRUE),
          uiOutput("sampleIconPartial", inline = TRUE),
          renderIcon("incorrect", width = 24, html = TRUE)
        ),
        br(),
        h2("Typesetting"),
        p("Vitae libero ipsum faucibus auctor bibendum \\(b_i\\)."),
        p(id = "lorem"),
        withMathJax(),
        actionButton("insertLaTeX", "Insert LaTeX"),
        actionButton("typeset", "Typeset"),
        p(),
        br(),
        actionButton("quit", "Quit")
      )
    )
  )
)

# Define server logic required
server <- function(input, output, session) {
  
  # output$output1 <- renderText({ input$focusedElement })
  
  message("\nrLocker status")
  message_for_status(connection$status)
  message("\n")
  
  observeEvent(input$insertLaTeX, {
    insertUI(
      selector = "#lorem",
      where = "beforeEnd",
      ui = span(" \\(\\LaTeX\\)")
    )
  })
  
  observeEvent(input$typeset, typesetMath(session))
  observeEvent(input$quit, stopApp())
  
  output$sampleIconCorrect <- renderIcon("correct")
  output$sampleIconPartial <- renderIcon("partial")
  
  # Note: isolate() is not normally needed.
  isolate({
    stmt <- boastUtils::generateStatement(
      session,
      verb = "answered",
      object = "SAMPLE_QUESTION_ID",
      description = "SAMPLE_QUESTION_OUTPUT",
      interactionType = "choice",
      response = "SAMPLE_RESPONSE",
      success = TRUE,
      score = list(
        min = 0,
        max = 100,
        raw = 35,
        scaled = 0.35
      ),
      completion = FALSE,
      extensions = list(
        ref = "https://w3id.org/xapi/cmi5/result/extensions/progress",
        value = "100"
      )
    )
    #message(paste("Sample xAPI data: \n", stmt))
  })
  
}

# Run the application
boastApp(ui = ui, server = server, config = list(log = TRUE))

####
# Shiny Example
####
library(shiny)
library(DT)
#UI

ui <- navbarPage(
  "Particle Swarm Optimization",
  # tabPanel("Functionality of Algorithm", icon=icon("info"),
  #          h3("Hier wird die Funktionalitaet des Algoithmus beschrieben"),
  #          hr(),
  #          br(),
  #          #DT::dataTableOutput("myTable")
  # ),
  tabPanel(
    "Visualization of Algorithm",
    icon = icon("glyphicon glyphicon-eye-open", lib = "glyphicon"),
    h3("Interactive Graph"),
    sidebarLayout(
      sidebarPanel(
        numericInput(
          inputId = "inertia",
          label = "Inerita",
          value = 1,
          min = 0,
          max = 1,
          step = 0.1
        ),
        selectInput(
          inputId = "function_selected",
          label = "Base Function",
          choices = list("f1" = "f1", "f2" = "f2")
        ),
        numericInput(
          inputId = "n_particles",
          label = "Number of Particles",
          value = 100,
          min = 1,
          max = 1000
        ),
        selectInput(
          inputId = "auto_coef",
          label = "Auto-Coefficient",
          choices = list("On" = TRUE, "Off" = FALSE),
          selected = TRUE
        ),
        selectInput(
          inputId = "norm_arrows",
          label = "Normalize Arrows",
          choices = list("On" = TRUE, "Off" = FALSE),
          selected = FALSE
        ),
        sliderInput(
          inputId = "iter",
          label = "Iterations",
          value = 1,
          min = 1,
          max = 101,
          step = 1,
          animate = animationOptions(interval = 100)
        )
        
      ),
      mainPanel(# h4(textOutput("render_optim")),
        plotOutput(
          "render_particles", width = 500, height = 500
        ))
    )
  )
)

#================
#Server

server <- function(input, output, session) {
  
  max_iterations <- 100
  
  #initalize pso
  pso  = reactive({
    
    #returns message if test fails
    validate(need(!is.null(input$inertia), "inertia is null"))
    #initial plot (iter = 0)
    init_pso(
      max_iterations,
      input$n_particles,
      as.logical(input$auto_coef),
      input$inertia,
      as.logical(input$norm_arrows)
    )
  })
  
  #plot current pso state
  pso_output = reactive({
    run_pso(
      input$iter,
      max_iterations,
      input$n_particles,
      as.logical(input$auto_coef),
      input$inertia ,
      as.logical(input$norm_arrows)
    )
  })
  
  output$render_optim = renderText({
    print(pso_output()$g_best)
  })
  
  output$render_particles = renderPlot({
    if (input$iter == pso()$iter + 1) {
      #moves particles, updates bests, updates coefficients 
      pso()$next_i()
    } else {
      pso() <- pso_output()
    }
    # plot(pso$particles[ ,1], pso$particles[ ,2], xlim=c(-5, 5), ylim=c(-5, 5))
    pso()$plot_state()
  })
}


shinyApp(ui, server)
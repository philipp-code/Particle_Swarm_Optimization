#================= "VISUALIZATION OF ALGORITHM" functionality ============================
visualization_tab = tabPanel(
  
  "Visualization of Algorithm",
  icon = icon("glyphicon glyphicon-eye-open", lib = "glyphicon"),
  br(),
  fluidRow(
    
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
          choices = list(
            "Himmelblau" = "Himmelblau",
            "Rosenbrock" = "Rosenbrock",
            "Rastrigin" = "Rastrigin",
            "Eggholder" = "Eggholder"
          )
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
      
      mainPanel(fluidRow(column(
        12,
        align = "center",
        plotOutput("render_particles", height = "auto", width = "70%")
      )))
    )
  )
)
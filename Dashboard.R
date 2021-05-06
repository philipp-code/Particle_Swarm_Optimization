####
# Shiny Example
####
library(shiny)
library(DT)
#UI

library(plotly)
source("function_gallery_plots.R")
source("PSO.R")

#UI

ui <- fluidPage(

  
  titlePanel(
    fluidRow(
      column(9, br(), "Particle Swarm Optimization"), 
      column(3, img(height = 110, src = "dhbw_logo.png"))
    )
    
    ),
  
  tabsetPanel(
    tabPanel(
      "Introduction",
      icon = icon("info"),

      column(1),
      
      column(3,
             br(), br(),
             uiOutput("process_step_n")),
      
      column(
        3,
        br(),
        actionButton("process_b", "Back"),
        actionButton("process_f", "Forward")
      ),
      column(4)
      
    ),
    

    tabPanel(
      "Gallery",
      icon = icon("photo"),
      
      h3("Function Gallery"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "function_type_select",
            label = "Type of Function:",
            choices = c("Continuous", "Non-Continuous")
          ),
          
          selectInput(
            inputId = "function_select",
            label = "Function:",
            choices = character(0)
            
          ),
          selectInput(
            inputId = "color_select",
            label = "Color:",
            choices = c("YlOrRd", "YlGnBu", "viridis", "RdYlGn", "Spectral")
          ),
          
          sliderInput(
            inputId = "alpha_select",
            label = "Opacity:",
            value = 0.9,
            min = 0.0,
            max = 1.0
          )
          
        ),
        
        mainPanel(fluidRow(
          column(6,
                 plotlyOutput("plot_surface")),
          column(6,
                 plotlyOutput("plot_contour"))
        ))
      )
    ),
    
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
    ))
))


#================
#Server

server <- function(input, output, session) {
  
  #=================== Introduction ===================================
  step_counter <- reactiveValues(process_step = 1)
  
  observeEvent(input$process_f, {
    # button to go forward
    if (step_counter$process_step > 6) {
      step_counter$process_step <- 1
    } else {
      step_counter$process_step <-
        step_counter$process_step + 1     # if the add button is clicked, increment the value by 1 and update it
    }
  })
  
  observeEvent(input$process_b, {
    # button to go backward
    if (step_counter$process_step < 2) {
      step_counter$process_step <- 7
    } else {
      step_counter$process_step <-
        step_counter$process_step - 1     # if the add button is clicked, decrement the value by 1 and update it
    }
  })
  
  output$process_step_n <- renderUI({
    if (step_counter$process_step == 1) {
      img(src = "process.png", height = 400)
      
    } else if (step_counter$process_step == 2) {
      img(src = "step_1.png", height = 400)
      
    } else if (step_counter$process_step == 3) {
      img(src = "step_2.png", height = 400)
      
    } else if (step_counter$process_step == 4) {
      img(src = "step_3.png", height = 400)
      
    } else if (step_counter$process_step == 5) {
      img(src = "step_4.png", height = 400)
      
    } else if (step_counter$process_step == 6) {
      img(src = "step_5.png", height = 400)
      
    } else if (step_counter$process_step == 7) {
      img(src = "step_6.png", height = 400)
      
    }}
  )
  
  #================= "VISUALIZATION OF ALGORITHM" functionality ============================  
    
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
      pso() <<- pso_output()
    }
    # plot(pso$particles[ ,1], pso$particles[ ,2], xlim=c(-5, 5), ylim=c(-5, 5))
    pso()$plot_state()
  })
  
  #================= Gallery functionality ============================
  
  observe({
    # change function input based on function type -> to Non-Continuous
    x <- input$function_type_select
    
    if (x == "Non-Continuous")
      
      updateSelectInput(session,
                        "function_select",
                        choices = c("Function 1", "Function 2", "Function 3"))
  })
  
  observe({
    # change function input based on function type to Continuous
    x <- input$function_type_select
    
    if (x == "Continuous")
      
      updateSelectInput(
        session,
        "function_select",
        choices = c("Himmelblau", "Rosenbrock", "Rastrigin", "Eggholder")
      )
  })
  
  output$plot_surface <- renderPlotly({
    z <- generate_gallery_plot(input)
    
    fig1 <- plot_ly(
      z = ~ z,
      colors = input$color_select,
      alpha = input$alpha_select,
      showscale = FALSE
    ) %>% add_surface()
    
  })
  
  output$plot_contour <- renderPlotly({
    z <- generate_gallery_plot(input)
    
    fig2 <-
      plot_ly(
        z = ~ z,
        type = "contour",
        colors = input$color_select,
        alpha = input$alpha_select,
        showscale = FALSE
      )
    
  })
  
}

shinyApp(ui, server)
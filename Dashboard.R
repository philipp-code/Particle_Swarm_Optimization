####
# Shiny Example
####
library(shiny)
library(DT)
library(shinyWidgets)
library(shinydashboard) # use for box
library(shinycssloaders) # use for loading icon
library(metaheuristicOpt) # use for optim functions
library(hash) # use for hashmap
#UI
library(shinythemes)
library(plotly)
source("function_gallery_plots.R")
source("PSO.R")

#UI

ui <- fluidPage(
  theme = shinytheme("united"),
  useShinydashboard(),
  
  titlePanel(fluidRow(
    column(9, br(), "Particle Swarm Optimization"),
    column(3, img(height = 110, src = "dhbw_logo.png"))
  )),
  
  tabsetPanel(
    #=================== Introduction ===================================
    
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
    
    #================= Gallery functionality ============================
    
    tabPanel(
      "Function Gallery",
      icon = icon("photo"),
      br(),
      
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
            choices = c("Yellow-Red" = "YlOrRd", "Yellow-Blue" = "YlGnBu", 
                        "Darkblue-Yellow" = "viridis", "Red-Yellow-Green" = "RdYlGn", "Red-Yellow-Blue" = "Spectral")
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
                 plotlyOutput("plot_surface") %>% withSpinner(color="lightblue")),
          column(6,
                 plotlyOutput("plot_contour") %>% withSpinner(color="lightblue"))
        ))
      )
    ),
    
    
    #================= "VISUALIZATION OF ALGORITHM" functionality ============================
    
    tabPanel(
      
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
    ),
    
    #================= Comparison ============================
    
      tabPanel("Optimization Comparison",
      icon = icon("chart-bar"),
      br(),
  sidebarLayout(
    sidebarPanel(

      selectInput(
        inputId = "c_function",
        label = "Function:",
        choices = c("Himmelblau", "Rosenbrock", "Rastrigin", "Eggholder"),
        selected = FALSE
      ),
      
      sliderInput(
        inputId = "c_iterations",
        label = "Maximum Iterations:",
        value = 100,
        min = 1,
        max = 1000,
        step = 1
      ),
      sliderInput(
        inputId = "c_populations",
        label = "Number of Populations:",
        value = 5,
        min = 4,
        max = 30,
        step = 1
      ),
      sliderInput(
        inputId = "c_variables",
        label = "Number of Variables:",
        value = 5,
        min = 2,
        max = 100
      )
      #actionButton("play_minimize",label = "Minimize", icon("play"), width = '100%')
          
        ),
        mainPanel(
          br(),
          fluidRow(
            valueBoxOutput("pso_box", width = 6) %>% withSpinner(color="lightblue"),
            valueBoxOutput("abc_box", width = 6) %>% withSpinner(color="lightblue")
            
          ),
          
          fluidRow(
            valueBoxOutput("ga_box", width = 6) %>% withSpinner(color="lightblue"),
            valueBoxOutput("gbs_box", width = 6) %>% withSpinner(color="lightblue")
          ),
          
          fluidRow(
            valueBoxOutput("gwo_box", width = 6) %>% withSpinner(color="lightblue"),
            valueBoxOutput("ffa_box", width = 6) %>% withSpinner(color="lightblue")
          )
          
          
        )
        
      )
    )
  )
)

#=========================================================
#======================= Server ==========================
#=========================================================

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
      
    }
  })
  
  #================= "VISUALIZATION OF ALGORITHM" functionality ============================
  
  max_iterations <- 100
  #initalize pso
  pso = NULL
  
  #    reactive({
  #    #returns message if test fails
  #    validate(need(!is.null(input$inertia), "inertia is null"))
  #    #initial plot (iter = 0)
  #   init_pso(
  #      max_iterations,
  #      input$n_particles,
  #      as.logical(input$auto_coef),
  #      input$inertia,
  #      as.logical(input$norm_arrows)
  #    )
  #  })
  
  
  #plot current pso state
  pso_output = reactive({
    validate(need(input$inertia == 1, "inertia is null"))
    
    
    run_pso(
      input$iter,
      max_iterations,
      input$n_particles,
      as.logical(input$auto_coef),
      input$inertia ,
      as.logical(input$norm_arrows),
      input$function_selected
    )
  })
  
  output$render_optim = renderText({
    print(pso_output()$g_best)
  })
  
  output$render_particles = renderPlot({
    if (is.null(pso) || (input$iter != pso$iter + 1)) {
      pso <<- pso_output()
    } else {
      #moves particles, updates bests, updates coefficients
      pso$next_i()
    }
    # plot(pso$particles[ ,1], pso$particles[ ,2], xlim=c(-5, 5), ylim=c(-5, 5))
    pso$plot_state()
  },
  height = function() {
    session$clientData$output_render_particles_width
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
    req(input$function_select)
    z <- generate_gallery_plot(input$function_select)
    
    fig1 <- plot_ly(
      z = ~ z,
      colors = input$color_select,
      alpha = input$alpha_select,
      showscale = FALSE
    ) %>% add_surface()
    
  })
  
  output$plot_contour <- renderPlotly({
    req(input$function_select)
    z <- generate_gallery_plot(input$function_select)
    
    fig2 <-
      plot_ly(
        z = ~ z,
        type = "contour",
        colors = input$color_select,
        alpha = input$alpha_select,
        showscale = FALSE
      )
    
  })
  
  #================= Comparison functionality ============================
  
  fun <- reactive({
     generate_comparison_function(input$c_function)[[1]]
    })
  
  rangeVar <- reactive({
    out_lims<- c(generate_comparison_function(input$c_function)[[2]], generate_comparison_function(input$c_function)[[3]])
    matrix(out_lims, nrow=2)
    })
  

  opti_results <- reactive({
    
    results <- hash()
    r_fun <- fun()
    
    resultPSO <- PSO(r_fun, optimType="MIN", numVar=input$c_variables, numPopulation=input$c_populations,
                     maxIter=input$c_iterations, rangeVar = rangeVar())
    resultGBS <- GBS(r_fun, optimType = "MIN", numVar=input$c_variables, numPopulation = input$c_populations,
                     maxIter = input$c_iterations, rangeVar = rangeVar(), gravitationalConst = max(rangeVar()),
                     kbest = 0.1)
    resultABC <- ABC(r_fun, optimType="MIN", numVar=input$c_variables, numPopulation=input$c_populations,
                     maxIter=input$c_iterations, rangeVar = rangeVar())
    resultGA <- GA(r_fun, optimType="MIN", numVar=input$c_variables, numPopulation=input$c_populations,
                   maxIter=input$c_iterations, rangeVar = rangeVar())
    resultGWO <- GWO(r_fun, optimType="MIN", numVar=input$c_variables, numPopulation=input$c_populations,
                     maxIter=input$c_iterations, rangeVar = rangeVar())
    resultFFA <- FFA(r_fun, optimType="MIN", numVar=input$c_variables, numPopulation=input$c_populations,
                     maxIter=input$c_iterations, rangeVar = rangeVar())
    
    results[["PSO"]] <- round(r_fun(resultPSO), digits = 4)
    results[["GBS"]] <- round(r_fun(resultGBS), digits = 4)
    results[["ABC"]] <- round(r_fun(resultABC), digits = 4)
    results[["GA"]] <- round(r_fun(resultGA), digits = 4)
    results[["GWO"]] <- round(r_fun(resultGWO), digits = 4)
    results[["FFA"]] <- round(r_fun(resultFFA), digits = 4)
    
    results
  })
  
  output$pso_box <- renderValueBox({
    
    valueBox(
      
        h4("Particle Swarm Optimization"),
      color = colorpicker(opti_results()[["PSO"]],opti_results()),
      h2("MIN: ", opti_results()[["PSO"]])
      )
      
      
  })
  
  output$gbs_box <- renderValueBox(
    valueBox(
      h4("Gravitational Based Search"),
      color = colorpicker(opti_results()[["GBS"]],opti_results()),
      h2("MIN: ", opti_results()[["GBS"]])
    ))
  
  output$abc_box <- renderValueBox(
    valueBox(
      h4("Artificial Bee Colony"),
      color = colorpicker(opti_results()[["ABC"]],opti_results()),
      h2("MIN: ", opti_results()[["ABC"]])
    ))
  
  output$ga_box <- renderValueBox(
    valueBox(
      h4("Genetic Algorithm"),
      color = colorpicker(opti_results()[["GA"]],opti_results()),
      h2("MIN: ", opti_results()[["GA"]])
    ))
  
  output$gwo_box <- renderValueBox(
    valueBox(
      h4("Grey Wolf Optimize"),
      color = colorpicker(opti_results()[["GWO"]],opti_results()),
      h2("MIN: ", opti_results()[["GWO"]])
    ))
  
  output$ffa_box <- renderValueBox(
    valueBox(
      h4("Firefly Algorithm"),
      color = colorpicker(opti_results()[["FFA"]],opti_results()),
      h2("MIN: ", opti_results()[["FFA"]])
    ))
  
  
}

shinyApp(ui, server)
library(plotly)
library(metaheuristicOpt) # use for optim functions
library(hash) # use for hashmap

source("util/PSO.R")
source("util/function_gallery_plots.R")
#=========================================================
#======================= Server ==========================
#=========================================================
server <- function(input, output, session) {
  
  #=================== Introduction ===================================
  step_counter <- reactiveValues(process_step = 1)
  
  observeEvent(input$process_start, {
    step_counter$process_step <- 2
    
  })
  
  
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
      img(src = "process.png", height = 500)
      
    } else if (step_counter$process_step == 2) {
      img(src = "step_1.png", height = 500)
      
    } else if (step_counter$process_step == 3) {
      img(src = "step_2.png", height = 500)
      
    } else if (step_counter$process_step == 4) {
      img(src = "step_3.png", height = 500)
      
    } else if (step_counter$process_step == 5) {
      img(src = "step_4.png", height = 500)
      
    } else if (step_counter$process_step == 6) {
      img(src = "step_5.png", height = 500)
      
    } else if (step_counter$process_step == 7) {
      img(src = "step_6.png", height = 500)
      
    }
  })
  
  output$process_step_e <- renderUI({
    
    
    if (step_counter$process_step == 1) {
      img(src = "cover.jpg", height = 500)
      
    } else if (step_counter$process_step == 2) {
      img(src = "1_example.png", height = 500)
      
    } else if (step_counter$process_step == 3) {
      img(src = "2_example.png", height = 500)
      
    } else if (step_counter$process_step == 4) {
      img(src = "3_example.png", height = 500)
      
    } else if (step_counter$process_step == 5) {
      img(src = "4_example.png", height = 500)
      
    } else if (step_counter$process_step == 6) {
      img(src = "5_example.png", height = 500)
      
    } else if (step_counter$process_step == 7) {
      img(src = "2_example.png", height = 500)
      
    }}
  )
  
  output$explanation_box <- renderValueBox({
    
    color = "aqua"
    
    if (step_counter$process_step == 1) {
      
      valueBox(
        color = color,
        h4("Particle Swarm Optimization"),
        h2("The PSO algorithm is a stochastic optimization technique. It simulates animal's 
            social behavior cooperating with each in a swarm in order to find food.")
      )
      
    } else if (step_counter$process_step == 2) {
      
      valueBox(
        color = color,
        h4("Generate initial particle:"),
        h2("Starting positions of 
        the particles are distributed over the whole room: In this example
        we have three people who want to find the minimum in a mountain region")
      )
      
    } else if (step_counter$process_step == 3) {
      
      valueBox(
        color = color,
        h4("Evaluate the fitness function (local level):"),
        h2("At each time step each particle computes
        the value of the fitness function at it's current position: Each person walks for example
        5 km in every of the three directions and gets a new position.
        If this new position is better than the personal or team best loaction the person needs 
        to update it's recall, if not, the person doesn't need to take any action")
      )
      
    } else if (step_counter$process_step == 4) {
      img(src = "Example_3.png", height = 400)
      
      valueBox(
        color = color,
        h4("Update personal and global best(local level):"),
        h2("compares that value (from the 
        step before) to it's previous best value, and if it's greater-> updates it: We assume that the new position is
        not better than the personal or teams best location, so the person starts from the new position. The person
        walks again 5 km in each direction.")
      )
      
    } else if (step_counter$process_step == 5) {
      
      valueBox(
        color = color,
        h4("Update velocity and position of particle (vicinity level):"),
        h2("We look at a small set of particels, in some way it changes his velocity
        we are taking the sum of the behaviours we just had before:If we change the distance to 10 km,
        the person will end up somewhere in this grey area. 
        You can see the areas for the first distance and for the second distance.")
      )
      
    } else if (step_counter$process_step == 6) {
      
      valueBox(
        h4("Conversion Criteria met-Yes(global level): "),
        color = "light-blue",
        h2("The whole system takes it best til know -> This is already the best possible"),
        color = color,
        h4("Conversion Criteria met-Yes (global level): "),
        h2("The whole system takes it best til know -> This is already the best possible")
      )
      
    } else if (step_counter$process_step == 7) {
      
      valueBox(
        color = color,
        h4("Conversion Criteria met-No (global level):"),
        h2("It is not the best possible, we need to do it again")
      )
      
    }
  })
  
  #================= "VISUALIZATION OF ALGORITHM" functionality ============================
  
  max_iterations <- 100
  #initalize pso
  pso = NULL
  
  
  #plot current pso state
  pso_output = reactive({
    validate(need(input$inertia > 0, "inertia is null"))
    
    
    run_pso(
      input$iter,
      max_iterations,
      input$n_particles,
      as.logical(input$auto_coef),
      input$coef_1,
      input$coef_2,
      input$inertia,
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
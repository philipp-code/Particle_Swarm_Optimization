library(plotly)
library(metaheuristicOpt) # use for optim functions
library(hash) # use for hashmap

source("util/PSO.R")
source("util/function_gallery_plots.R")

#=========================================================
#======================= Server ==========================
#=========================================================

server <- function(input, output, session) {
  
  #====================================================================
  # Title: Introduction
  # Author: Katerina Matysova, Julia Albrecht, Philipp Schneider
  #====================================================================
  
  # info button
  observeEvent(input$intro_info_button, {
    sendSweetAlert(
      session,
      title = "How to navigate",
      text = "Here you find a description of the algorithm.
      Use the buttons to scroll through individual steps of the process.",
      btn_colors = "#d73925"
    )
  })
  
  # process step count
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
  
  # insert process picture based on step variable
  output$process_step_n <- renderUI({
    width = "100%"
    
    if (step_counter$process_step == 1) {
      img(src = "process.png", width = width)
      
    } else if (step_counter$process_step == 2) {
      img(src = "step_1.png", width = width)
      
    } else if (step_counter$process_step == 3) {
      img(src = "step_2.png", width = width)
      
    } else if (step_counter$process_step == 4) {
      img(src = "step_3.png", width = width)
      
    } else if (step_counter$process_step == 5) {
      img(src = "step_4.png", width = width)
      
    } else if (step_counter$process_step == 6) {
      img(src = "step_5.png", width = width)
      
    } else if (step_counter$process_step == 7) {
      img(src = "step_6.png", width = width)
      
    }
  })
  
  # insert description picture based on step variable
  output$process_step_e <- renderUI({
    width = "100%"
    
    if (step_counter$process_step == 1) {
      img(src = "cover.jpg", width = width)
      
    } else if (step_counter$process_step == 2) {
      img(src = "1_example.png", width = width)
      
    } else if (step_counter$process_step == 3) {
      img(src = "2_example.png", width = width)
      
    } else if (step_counter$process_step == 4) {
      img(src = "3_example.png", width = width)
      
    } else if (step_counter$process_step == 5) {
      img(src = "4_example.png", width = width)
      
    } else if (step_counter$process_step == 6) {
      img(src = "5_example.png", width = width)
      
    } else if (step_counter$process_step == 7) {
      img(src = "2_example.png", width = width)
      
    }
  })
  
  # insert explanation based on step variable
  output$explanation_box <- renderValueBox({
    color = "aqua"
    
    if (step_counter$process_step == 1) {
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "The PSO algorithm is a stochastic optimization technique. It simulates animals'
          social behavior cooperating with each other in a swarm in order to find food.
          
          "
        ))
        )
      
    } else if (step_counter$process_step == 2) {
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "Starting positions of the particles P are distributed across the area of interest. To visualize, 
          here we have three people searching for the lowest valley in a mountain region.
          \n $$\\vec{P_i} = \\left[\\vec{p_{0,i}}, \\vec{p_{1,i}}, ..., \\vec{p_{n,i}}\\right]$$
          Each of these particles is in movement with a velocity allowing them to update 
          their position over the iterations to find the global minimum.
           \n $$\\vec{V_i} = \\left[\\vec{v_{0,i}}, \\vec{v_{1,i}}, ..., \\vec{v_{n,i}}\\right]$$"
        ))
        )
      
    } else if (step_counter$process_step == 3) {
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "At each time step the particles compute the value of the fitness function at their current position. For example, each person walks
          5 km in every of the three directions and gets a new position.
          Generally, the next position is calculated by adding the new velocity to the current position of the particle:
           $$\\vec{P_{i+1}} = \\vec{P_{i}} + \\vec{V_{i+1}}$$
          The new velocity is calculated by adding up the different vectors:
           $$\\vec{V_{i+1}} = {\\omega}\\vec{V_{i}} + c_1r_1\\left(\\vec{P_{best(i)}} - \\vec{P_{i}}\\right)+$$
           $$c_2r_2\\left(\\vec{g_{best}} - \\vec{P_{i}}\\right)$$
           Parameters:
           The intertia $$\\text{}  {\\omega}\\in R^+ $$
           defines the ability of the swarm to change its direction.
           The coefficients for local and global best 
           $$c_1,c_2\\in R^+$$
           define the influence of these vectors.
           The weights $$r_1,r_2\\in [0,2]$$ 
           are defined randomly.
          "
        ))
        )
      
    } else if (step_counter$process_step == 4) {
      img(src = "Example_3.png", height = 400)
      
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "The particles compare the new positions to their previous best value and if it's greater, they update it. If the new position is
          not better than the personal or the team's best location, the person starts from the new position and 
          walks again 5 km in each direction.
          "
        ))
        )
      
    } else if (step_counter$process_step == 5) {
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "We look at how a small set of particels change their velocity by taking the sum of behaviors 
          described before. If we change the distance to 10 km, the person will end up somewhere in the grey area.
          Here you can see the areas for the first distance and for the second distance."
        ))
        )
      
    } else if (step_counter$process_step == 6) {
      valueBox(
        subtitle = "",
        withMathJax(h2(
          "The whole team takes its best until now and if it's the best possible, the algorithm converges. We see 
          that if the whole team works together, they will find the valley in the mountain region."
        )),
        color = color
        
        )
      
    } else if (step_counter$process_step == 7) {
      valueBox(
        color = color,
        subtitle = "",
        withMathJax(h2(
          "If it is not the best possible, the team has to start the search again.")
        ))
      
    }
  })

  #====================================================================
  # Title: VISUALIZATION OF ALGORITHM
  # Author: Frederik Dammeier, Philipp Schneider
  #====================================================================
  
  observeEvent(input$vis_info_button, {
    sendSweetAlert(
      session,
      title = "How to navigate",
      text = "On this page the algorithm comes alive.
      Change input parameters on the left and start the iterations. Now you will see the
      particles searching for the optimum.",
      btn_colors = "#d73925"
    )
  })
  
  
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
    # to optimize animation performance, we check whether we can just go one step further
    # in the optimization (pso$next_i) or if we need to do a full rerun (run_pso())
    if (is.null(pso) || (input$iter != pso$iter + 1)) {
      # do full rerun
      pso <<- pso_output()
    } else {
      # iterate one step
      # moves particles, updates bests, updates coefficients
      pso$next_i()
    }
    # plot the algorithms current state
    pso$plot_state()
  },
  height = function() {
    session$clientData$output_render_particles_width
  })
  
  #====================================================================
  # Title: Function Gallery
  # Author: Katerina Matysova
  #====================================================================
  
  # info button
  observeEvent(input$gallery_info_button, {
    sendSweetAlert(
      session,
      title = "How to navigate",
      text = "Visualize different functions in this gallery.
      You can change the function, color, and opacity to your liking. Feel free to take a screenshot
      with the camera icon above the plot.",
      btn_colors = "#d73925"
    )
  })
  
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
  
  # create 3D Plot
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
  
  # create contour plot
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
  
  #====================================================================
  # Title: Comparison
  # Author: Katerina Matysova
  #====================================================================
  
  # info button
  observeEvent(input$compare_info_button, {
    sendSweetAlert(
      session,
      title = "How to navigate",
      text = "Adjust the input parameters on the left to see how
      different algorithms compare to each other. The best ones will turn green
      and the worst ones red. Note that with high parameter values the
      calculations may take a while.",
      btn_colors = "#d73925"
    )
  })
  
  # prepare input before activate button is pressed
  output_staging <- reactiveValues()
  
  output_staging$opti_results <- ({
    results = hash()
    results[["PSO"]] <- "-"
    results[["GBS"]] <- "-"
    results[["ABC"]] <- "-"
    results[["GA"]]  <- "-"
    results[["GWO"]] <- "-"
    results[["FFA"]] <- "-"
    
    results
  })
  
  # start calculation on button press
  observeEvent(input$play_minimize, {
    update_results()
  })
  
  fun <- reactive({
    # get function
    generate_comparison_function(input$c_function)[[1]]
  })
  
  rangeVar <- reactive({
    out_lims <-
      c(
        # get function area
        generate_comparison_function(input$c_function)[[2]],
        generate_comparison_function(input$c_function)[[3]]
      )
    matrix(out_lims, nrow = 2)
  })
  
  update_results <- function() {
    output_staging$opti_results <- ({
      
      # create progress bar
      withProgress(message = 'Calculating...', value = 0, {
        number_steps = 8
        incProgress(1 / number_steps)
        
        results <- hash()
        r_fun <- fun()
        
        # for each algorithm, get the result
        resultPSO <-
          PSO(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar()
          )
        incProgress(1 / number_steps)
        resultGBS <-
          GBS(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar(),
            gravitationalConst = max(rangeVar()),
            kbest = 0.1
          )
        incProgress(1 / number_steps)
        resultABC <-
          ABC(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar()
          )
        incProgress(1 / number_steps)
        resultGA <-
          GA(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar()
          )
        incProgress(1 / number_steps)
        resultGWO <-
          GWO(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar()
          )
        incProgress(1 / number_steps)
        resultFFA <-
          FFA(
            r_fun,
            optimType = "MIN",
            numVar = input$c_variables,
            numPopulation = input$c_populations,
            maxIter = input$c_iterations,
            rangeVar = rangeVar()
          )
        incProgress(1 / number_steps)
        
        # store results in hash map
        results[["PSO"]] <- round(r_fun(resultPSO), digits = 4)
        results[["GBS"]] <- round(r_fun(resultGBS), digits = 4)
        results[["ABC"]] <- round(r_fun(resultABC), digits = 4)
        results[["GA"]] <- round(r_fun(resultGA), digits = 4)
        results[["GWO"]] <- round(r_fun(resultGWO), digits = 4)
        results[["FFA"]] <- round(r_fun(resultFFA), digits = 4)
        
        incProgress(1 / number_steps)
        
        results
        
      })
    })
  }
  
  # create output box for each algorithm
  output$pso_box <- renderInfoBox({
    infoBox(
      h4("Particle Swarm Optimization"),
      icon = icon("dove"),
      color = colorpicker(
        output_staging$opti_results[["PSO"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["PSO"]])
    )
  })
  
  output$gbs_box <- renderInfoBox({
    infoBox(
      h4("Gravitational Based Search"),
      icon = icon("grav"),
      color = colorpicker(
        output_staging$opti_results[["GBS"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["GBS"]])
    )
  })
  
  output$abc_box <- renderInfoBox({
    infoBox(
      h4("Artificial Bee Colony"),
      icon = icon("forumbee"),
      color = colorpicker(
        output_staging$opti_results[["ABC"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["ABC"]])
    )
  })
  
  output$ga_box <- renderInfoBox({
    infoBox(
      h4("Genetic Algorithm"),
      icon = icon("dna"),
      color = colorpicker(
        output_staging$opti_results[["GA"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["GA"]])
    )
  })
  
  output$gwo_box <- renderInfoBox({
    infoBox(
      h4("Grey Wolf Optimize"),
      icon = icon("wolf-pack-battalion"),
      color = colorpicker(
        output_staging$opti_results[["GWO"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["GWO"]])
    )
  })
  
  output$ffa_box <- renderInfoBox({
    infoBox(
      h4("Firefly Algorithm"),
      icon = icon("bug"),
      color = colorpicker(
        output_staging$opti_results[["FFA"]],
        output_staging$opti_results
      ),
      h2("MIN: ", output_staging$opti_results[["FFA"]])
    )
  })
  
  
}
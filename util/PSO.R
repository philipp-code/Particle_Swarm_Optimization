# Sample function for algorithm testing.
sample_function = function(x, y) {
  x**2 + (y + 1) ** 2 - 5 * cos(1.5 * x + 1.5) - 5 * cos(2 * y - 1.5)
}

# This class contains the main particle swarm logic
# and a method to plot the algorithms current state.
# It is implemented as a RefClass so that it can be instantiated.
# pso can solve problems using two input variables (x, y)
#
# pso implementation inspired by https://t1p.de/y2pc
pso = setRefClass("pso", 
                  fields = list(
                    # the particles positions (x, y)
                    particles = "matrix",
                    # the particles velocities
                    velocities = "matrix",
                    # the function on which to evaluate the particles
                    # position
                    fitness_function = "function",
                    # a vector of c(min, max) which is used to limit both x and y.
                    search_space = "numeric",
                    # the algorithms inertia
                    w = "numeric",
                    # the algorithms cognitive "local" component
                    c_1 = "numeric",
                    # the algorithms cognitive "global" component
                    c_2 = "numeric",
                    # after how many iterations the search terminates
                    max_iter = "numeric",
                    # whether the coefficients should shift from local focus to
                    # global focus during the run
                    auto_coef = "logical",
                    # the number of particles
                    N = "numeric",
                    # the particles individual best positions (x, y)
                    p_bests = "matrix",
                    # the values of fitness_function at those positions
                    p_bests_values = "numeric",
                    # the swarms best position (x, y)
                    g_best = "numeric",
                    # the value of fitness_function at that position
                    g_best_value = "numeric",
                    # the step at which the algorithm is currently at
                    iter = "numeric",
                    # whether the algorithm has terminated.
                    is_running = "logical",
                    # plot space for the visualization
                    # for performance reasons
                    x_image = "matrix",
                    # precalculated function values for the visualization
                    z = "matrix",
                    # whether to normalize the arrows in the visualization
                    norm_arrow = "logical"
                  ),
                  methods = list(
                    # main logic
                    next_i = function(){
                      if (iter > 0) {
                        move_particles()
                        update_bests()
                        update_coef()
                      }
                      iter <<- iter + 1
                      # check if terminated
                      is_running <<- is_running && iter < max_iter
                      is_running
                    },
                    # update the coefficients if "auto_coef"=True
                    update_coef = function(){
                      if (auto_coef) {
                        t = iter
                        n = max_iter
                        w <<- (0.4 / n**2 ) * (t - 1) ** 2 + 0.4
                        c_1 <<- -3 * t / n + 3.5
                        c_2 <<- 3 * t / n + 0.5
                      }
                    },
                    # search logic
                    move_particles = function(){
                      # add inertia
                      new_velocities = w * velocities
                      
                      # add cognitive component
                      r_1 = runif(N, 0.0, 1.0)
                      r_1 = cbind(r_1, r_1)
                      new_velocities = new_velocities + c_1 * r_1 * (p_bests - particles)
                      
                      # add social component
                      r_2 = runif(N, 0.0, 1.0)
                      r_2 = cbind(r_2, r_2)
                      g_best_mat = matrix(g_best, ncol = 2, nrow = N,  byrow = TRUE)
                      new_velocities = new_velocities + c_2 * r_2 * (g_best_mat - particles)
                      
                      # check if optimum is reached
                      is_running <<- sum(velocities - new_velocities) != 0
                      
                      # update positions and velocities
                      velocities <<- new_velocities
                      particles <<- particles + new_velocities
                      
                      # limit particle positions to the search space ('nearest' approach; https://t1p.de/fjvh)
                      particles[particles < search_space[1]] <<- search_space[1]
                      particles[particles > search_space[2]] <<- search_space[2]
                    },
                    # optimization logic
                    update_bests = function(){
                      fits = fitness_function(particles[ ,1], particles[ ,2])
                      
                      for (i in 1:N) {
                        # update best personal value (cognitive)
                        if (all(fits[i] < p_bests_values[i])) {
                          p_bests_values[i] <<- fits[i]
                          p_bests[i, ] <<- particles[i, ]
                          # update best global value (social)
                          if (all(fits[i] < g_best_value)) {
                            g_best_value <<- fits[i]
                            g_best <<- particles[i, ]
                          }
                        }
                      }
                    },
                    # visualize the algorithms state
                    plot_state = function(){
                      # plot the background
                      image(x_image[,1], x_image[,2], z, xlab="x", ylab="y", 
                            # add some information as the graphs title
                            main = paste("Global Best = ", round(g_best_value, digits = 4),
                                         " at [", round(g_best[1], digits = 2),
                                         ", ", round(g_best[2], digits = 2), "]",
                                         "\nLocal Coef. = ", round(c_1, digits = 2),
                                         ", Global Coef. = ", round(c_2, digits = 2), sep = ""),
                            cex.main = 1)
                      
                      # extract x and y component from the velocity matrix
                      U = velocities[ ,1]
                      V = velocities[ ,2]
                      
                      #normalize error length
                      if(norm_arrow){
                        nor = sqrt(U**2+V**2)
                        U = U/nor
                        V = V/nor 
                      }
                      
                      # calculate the particles new position
                      x_pos_2 = particles[ ,1]+U
                      y_pos_2 = particles[ ,2]+V
                      
                      # if arrows are too short to draw them, arrows() throws a warning.
                      # to improve performance in later stages of the optimization run
                      # (where all arrows are very close to the global optimum), we
                      # suppress those warnings.
                      suppressWarnings(
                        # draw particles and their velocities as arrows
                        arrows(particles[ ,1], particles[ ,2], 
                                              x_pos_2, 
                                              y_pos_2, 
                                              length=0.1, col="darkslateblue"))
                    }
                  ))



# execute a full run of the algorithm until a given point (input_iter)
# return the pso object
run_pso = function(input_iter, max_iter, n_particles, input_auto_coef, input_c_1, input_c_2, input_inertia, input_arrows, input_function_selected){
  # init pso object
  pso_1 = init_pso(max_iter, n_particles, input_auto_coef, input_c_1, input_c_2, input_inertia, input_arrows, input_function_selected)
  
  # run pso$next_i()
  while (pso_1$iter < input_iter) {
    pso_1$next_i()
  }
  
  pso_1
}

# initialize the pso object without running any iterations.
init_pso = function(max_iter, input_n_particles, input_auto_coef, input_c_1, input_c_2, input_inertia, input_arrows, input_function_selected){
  # random seed
  set.seed(31)
  
  # general search area
  def_area = c(-6, 6)
  
  # select fitness function for user input
  if (input_function_selected == "Himmelblau") {
    fitness <- function(x, y) (x*x+y-11)**2 + (x+y*y-7)**2 # himmelblau
    def_area <- c(-60, 60)
    
    
  } else if (input_function_selected == "Rosenbrock") {
    
    fitness  <- function(x, y){ (1-x)**2 + 100* ((y-x**2))**2} # rosenbrock
    def_area = c(-2, 2)
    
  } else if (input_function_selected == "Rastrigin") {
    
    fitness <- function(x, y) (x**2 - 10 * cos(2 * pi * x)) + (y**2 - 10 * cos(2 * pi * y)) + 20 # rastrigin
    def_area = c(-5.12, 5.12)
    
    
  } else if (input_function_selected == "Eggholder") {
    fitness <- function(x, y) -(y+47) * sin(sqrt(abs(y+x/2+47))) - x * sin(sqrt(abs(x-(y+47)))) # eggholder
    def_area = c(-512, 512)
  }
  
  
  n_particles = input_n_particles
  auto_coef <-input_auto_coef
  
  #create image matrix from two sequences 
  x_image = matrix(c(seq(from=def_area[1],to=def_area[2],length.out=100),
                     seq(from=def_area[1],to=def_area[2],length.out=100)), ncol=2)
  
  #fill matrix-columns with values generated by the function
  z = outer(x_image[,1], x_image[,2], FUN = fitness) 
  
  # runif can be used to produce random numbers; runif does not stand for run if
  # randomly define particle start positions and velocities
  particles = cbind(runif(n = n_particles, min = def_area[1], max = def_area[2]),
                    runif(n = n_particles, min = def_area[1], max = def_area[2]))
  velocities = cbind((runif(n = n_particles, min = def_area[1], max = def_area[2]) - 0.5) / (def_area[2]-def_area[1]),
                     (runif(n = n_particles, min = def_area[1], max = def_area[2]) - 0.5) / (def_area[2]-def_area[1]))
  
  # initialize the pso object
  pso_1 = pso(particles = particles,
              velocities = velocities,
              fitness_function = fitness,
              search_space = def_area,
              w = input_inertia, c_1=input_c_1, c_2=input_c_2, max_iter=max_iter, auto_coef = auto_coef,
              N = nrow(particles), p_bests = particles,
              p_bests_values = fitness(particles[ ,1], particles[ ,2]),
              g_best = particles[1, ],
              g_best_value = fitness(particles[1, 1], particles[1, 2]),
              iter = 0, is_running = TRUE, x_image = x_image, z = z, norm_arrow = input_arrows)
  
  pso_1
}

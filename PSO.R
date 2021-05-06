fitness = function(x, y) {
  x**2 + (y + 1) ** 2 - 5 * cos(1.5 * x + 1.5) - 5 * cos(2 * y - 1.5)
}


pso = setRefClass("pso", 
                  fields = list(
                    particles = "matrix",
                    velocities = "matrix",
                    fitness_function = "function",
                    w = "numeric",
                    c_1 = "numeric",
                    c_2 = "numeric",
                    max_iter = "numeric",
                    auto_coef = "logical",
                    N = "numeric",
                    p_bests = "matrix",
                    p_bests_values = "numeric",
                    g_best = "numeric",
                    g_best_value = "numeric",
                    iter = "numeric",
                    is_running = "logical",
                    x_image = "matrix",
                    z = "matrix",
                    norm_arrow = "logical"
                  ),
                  methods = list(
                    next_i = function(){
                      if (iter > 0) {
                        move_particles()
                        update_bests()
                        update_coef()
                      }
                      iter <<- iter + 1
                      is_running <<- is_running && iter < max_iter
                      is_running
                    },
                    update_coef = function(){
                      if (auto_coef) {
                        t = iter
                        n = max_iter
                        w <<- (0.4 / n**2 ) * (t - 1) ** 2 + 0.4
                        c_1 <<- -3 * t / n + 3.5
                        c_2 <<- 3 * t / n + 0.5
                      }
                    },
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
                    },
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
                    
                    plot_state = function(){
                      image(x_image[,1], x_image[,2], z, xlab="x", ylab="y", 
                            main = paste("g_best = ", round(g_best_value, digits = 4),
                                         ", c_1 = ", round(c_1, digits = 4),
                                         ", c_2 = ", round(c_2, digits = 4), sep = "",
                                        ", auto_coef = ", auto_coef,
                                        ", Normalize Arrows = ", norm_arrow,
                                        ", inertia = ", w),
                            cex.main = 1)
                      # contour(x_image[,1], x_image[,2], z, nlevels=10, add=TRUE, col="grey50")
                      # points(particles[ ,1], particles[ ,2], pch=19, col="darkslateblue")
                      
                      
                      U = velocities[ ,1]
                      V = velocities[ ,2]
                      
                      #normalize error length
                      if(norm_arrow){
                        N <<- sqrt(U**2+V**2)
                        U = U/N
                        V = V/N 
                      }

                      suppressWarnings(arrows(particles[ ,1], particles[ ,2], 
                                              particles[ ,1]+U, 
                                              particles[ ,2]+V, 
                                              length=0.1, col="darkslateblue"))
                    }
                  ))




run_pso = function(input_iter, max_iter, n_particles, input_auto_coef, input_inertia, input_arrows){
  pso_1 = init_pso(max_iter, n_particles, input_auto_coef, input_inertia, input_arrows)
  
  while (pso_1$iter < input_iter) {
    pso_1$next_i()
  }
  
  pso_1
}

init_pso = function(max_iter, input_n_particles, input_auto_coef, input_inertia, input_arrows){
  
  set.seed(31)
  
  n_particles = input_n_particles
  auto_coef <-input_auto_coef
  
  #create matrix from to sequences 
  x_image = matrix(c(seq(from=-5,to=5,length.out=100),seq(from=-5,to=5,length.out=100)), ncol=2)
  #fill matrix-columns with values generated by the function
  z = outer(x_image[,1], x_image[,2], FUN = fitness) 
  
  #runif can be used to produce random numbers; runif does not stand for run if
  particles = cbind(runif(n = n_particles, min = -5, max = 5), runif(n = n_particles, min = -5, max = 5))
  velocities = cbind((runif(n = n_particles, min = -5, max = 5) - 0.5) / 10, (runif(n = n_particles, min = -5, max = 5) - 0.5) / 10)
  
  pso_1 = pso(particles = particles,
              velocities = velocities,
              fitness_function = fitness, 
              w = input_inertia, c_1=1, c_2=1, max_iter=max_iter, auto_coef = auto_coef,
              N = nrow(particles), p_bests = particles,
              p_bests_values = fitness(particles[ ,1], particles[ ,2]),
              g_best = particles[1, ],
              g_best_value = fitness(particles[1, 1], particles[1, 2]),
              iter = 0, is_running = TRUE, x_image = x_image, z = z, norm_arrow = input_arrows)
  
  pso_1
}

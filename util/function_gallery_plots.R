library(plotly)
generate_gallery_plot <- function(function_select){
  
  if (function_select == "Himmelblau") {
    fun <- function(x, y) (x*x+y-11)**2 + (x+y*y-7)**2 # himmelblau
    xs <- seq(-6, 6, by=0.1)
    ys <- seq(-6, 6, by = 0.1)
    
  } else if (function_select == "Rosenbrock") {
    
    fun <- function(x, y) (1-x)**2 + 100* ((y-x**2))**2 # rosenbrock
    xs <- seq(-2, 2, by=0.15)
    ys <- seq(-1, 3, by = 0.15)
    
  } else if (function_select == "Rastrigin") {
    
    fun <- function(x, y) (x**2 - 10 * cos(2 * pi * x)) + (y**2 - 10 * cos(2 * pi * y)) + 20 # rastrigin
    xs <- seq(-5.12, 5.12, by = 0.1)
    ys <- seq(-5.12, 5.12, by = 0.1)
    
  } else if (function_select == "Eggholder") {
    
    fun <- function(x, y) -(y+47) * sin(sqrt(abs(y+x/2+47))) - x * sin(sqrt(abs(x-(y+47)))) # eggholder
    xs <- seq(-512, 512, by = 15)
    ys <- seq(-512, 512, by = 15)
  }
  
  ### Non-Continuous Functions
  
  else if (function_select == "Function 1") {
    
    fun <- function(x, y) sin(1/sqrt(x^2+y^2)) # discont 1
    xs <- seq(-10, 10, by = 1)
    ys <- seq(-10, 10, by = 1)
    
  } else if (function_select == "Function 2") {
    
    fun <- function(x, y) (2*x*y)/(x^2+y^2) # discont 2
    xs <- seq(-10, 10, by = 1)
    ys <- seq(-10, 10, by = 1)
    
  } else if (function_select == "Function 3") {
    
    fun <- function(x, y) (x^2+y^2)*sin(1/(x^2+y^2)) # discont 3
    xs <- seq(-0.5, 0.5, by = 0.01)
    ys <- seq(-0.5, 0.5, by = 0.01)
    
  }
  
  z <- mapply(fun, list(xs), ys)
  
  return(z)
}

generate_comparison_function <- function(function_select){
  
  if (function_select == "Himmelblau") {
    fun <- function(x) (x[1]*x[1]+x[2]-11)**2 + (x[1]+x[2]*x[2]-7)**2 # himmelblau
    rangeVar <- matrix(c(-6,6), nrow=2)
    
  } else if (function_select == "Rosenbrock") {
    
    fun <- function(x) (1-x[1])**2 + 100* ((x[2]-x[1]**2))**2 # rosenbrock
    rangeVar <- matrix(c(-2,3), nrow=2) # different
    
  } else if (function_select == "Rastrigin") {
    
    fun <- function(x) (x[1]**2 - 10 * cos(2 * pi * x[1])) + (x[2]**2 - 10 * cos(2 * pi * x[2])) + 20 # rastrigin
    rangeVar <- matrix(c(-5.12,5.12), nrow=2)
    
  } else if (function_select == "Eggholder") {
 
    fun <- function(x) -(x[2]+47) * sin(sqrt(abs(x[2]+x[1]/2+47))) - x[1] * sin(sqrt(abs(x[1]-(x[2]+47)))) # eggholder
    rangeVar <- matrix(c(-512,512), nrow=2)
  }
  
  return(c(fun, rangeVar))
}



colorpicker <- function(num, resulthash) {
  
  if (num != "-") {
    val <- values(resulthash)
    
    if (num == min(val)) {
      return("green")
    } 
    else if (num == max(val)){
      return("red")
    }
    else {
      return("black")
    }
    
  } else {
    return("black")
  }
  

}

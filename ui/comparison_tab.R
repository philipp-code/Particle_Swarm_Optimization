#================= Comparison ============================

comparison_tab = tabPanel("Optimization Comparison",
         icon = icon("chart-bar"),
         br(),
         sidebarLayout(
           sidebarPanel(id="sidebar",
             
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
               min = 5,
               max = 50,
               step = 5
             ),
             sliderInput(
               inputId = "c_variables",
               label = "Number of Variables:",
               value = 5,
               min = 2,
               max = 100
             ),
             actionButton("play_minimize",label = "Minimize", icon("play"), width = '100%')
             
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
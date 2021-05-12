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
               value = 50,
               min = 50,
               max = 500,
               step = 10
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
               value = 10,
               min = 10,
               max = 100,
               step = 5
             ),
             actionButton("play_minimize",label = "  Minimize", icon("play"), width = '100%')
             
           ),
           mainPanel(
             fluidRow(
               actionButton("compare_info_button", icon("info"))
             ),
             br(), br(),
             fluidRow(
               infoBoxOutput("pso_box", width = 6) %>% withSpinner(color="red"),
               infoBoxOutput("abc_box", width = 6) %>% withSpinner(color="red")
               
             ),
             
             fluidRow(
               infoBoxOutput("ga_box", width = 6) %>% withSpinner(color="red"),
               infoBoxOutput("gbs_box", width = 6) %>% withSpinner(color="red")
             ),
             
             fluidRow(
               infoBoxOutput("gwo_box", width = 6) %>% withSpinner(color="red"),
               infoBoxOutput("ffa_box", width = 6) %>% withSpinner(color="red")
             )
             
             
           )
           
         )
)
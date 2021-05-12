
  #=================== Introduction ===================================
  
introduction_tab = tabPanel(
    "Introduction",
    icon = icon("info"),
    
    fluidRow(
    
    column(3,
           align = "center",
           br(), br(),
           uiOutput("process_step_n")),
    column(
      6,
      align = "center",
      br(),
      actionButton("process_start", label = "  Start Process", icon = icon("play-circle"),
                   style="color: #000000; background-color: #F5F5F5; border-color: #F5F5F5"),
      actionButton("process_b", icon("arrow-left",lib = "font-awesome"), 
                   style="color:	#000000; background-color: #F5F5F5; margin-left:10px; border-color: #F5F5F5"),
      actionButton("process_f", icon("arrow-right",lib = "font-awesome"),
                   style="color:	#000000; background-color: #F5F5F5; margin-left:10px; border-color: #F5F5F5"), 
      br(),
      br(),
      uiOutput("process_step_e")
    ),
    #column(1),
    column(3,
           align = "left",
           br(), br(), br(), br(),
           valueBoxOutput("explanation_box", width = "100%") %>% withSpinner(color="lightblue")
    )
  )
)

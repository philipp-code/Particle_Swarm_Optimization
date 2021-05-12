
  #=================== Introduction ===================================
  
introduction_tab = tabPanel(
    "Introduction",
    icon = icon("info"),
    
    mainPanel(
    
    column(3,
           align = "center",
           br(), br(),
           uiOutput("process_step_n")),
    column(
      6,
      align = "center",
      br(),
      actionButton("process_start", label = "  Start Process", icon = icon("play-circle"),
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
      actionButton("process_b", icon("arrow-left",lib = "font-awesome"), 
                   style="color: #fff; background-color: #337ab7; margin-left:10px; border-color: #2e6da4"),
      actionButton("process_f", icon("arrow-right",lib = "font-awesome"), 
                   style="color: #fff; background-color: #337ab7; margin-left:10px; border-color: #2e6da4"), #Quelle:https://stackoverflow.com/questions/33620133/change-the-color-of-action-button-in-shiny 
      
      br(),
      br(),
      uiOutput("process_step_e")
    ),
    #column(1),
    column(3,
           align = "center",
           br(), br(), br(), br(),
           valueBoxOutput("explanation_box", width = "100%") %>% withSpinner(color="lightblue")
    )
  )
  
)


#=================== Introduction ===================================

introduction_tab =
  tabPanel("Introduction",
           icon = icon("book-open"),
           
           fluidRow(
             column(3,
                    uiOutput("process_step_n"), align = "center"),
             column(
               5,
               actionButton(
                 "process_start",
                 label = "  Start Process",
                 icon = icon("play-circle")
               ),
               actionButton("process_b", icon("arrow-left", lib = "font-awesome"),
                            style = "margin-left:10px"),
               actionButton("process_f", icon("arrow-right", lib = "font-awesome"),
                            style = "margin-left:10px"),
               
               uiOutput("process_step_e"),
               align = "center"
             ),
             column(
               4,
               fluidRow(actionButton("intro_info_button", icon("info"))),
               valueBoxOutput("explanation_box", width = "70%")
               ,
               align = "left"
             )
             
           ))

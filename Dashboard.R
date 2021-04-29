####
# Shiny Example
####
library(shiny)
library(DT)
View(iris)
#UI

ui <- navbarPage("Test DHBW", 
                 tabPanel("Vizualization of Algorithm", icon=icon("info"),
                          h3("Das ist ein Test"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                inputId = "xcol",
                                label = "X Variable",
                                choices = names(iris)
                              ),
                              selectInput(
                                inputId = "ycol",
                                label = "Y Variable",
                                choices = c("Choose a variable" = "", names(iris))
                              ),
                              numericInput(
                                inputId = "clusters",
                                label = "Clusters count",
                                value = 3,
                                min = 1,
                                max = 9
                              ),
                              sliderInput(
                                inputId = "iter",
                                label = "PSO Iteration",
                                value = 1,
                                min = 1,
                                max = 101,
                                step = 1,
                                animate = animationOptions(interval = 100)
                              )
                            ),
                            mainPanel(
                              # h4(textOutput("render_optim")),
                              plotOutput("render_particles", width = 500, height = 500)
                            )
                          )
                 ),
                 tabPanel("Functionality of Algorithm", icon=icon("robot"),
                          h3("Hier wird die Funktionalitaet des Algoithmus beschrieben"),
                          hr(),
                          br(),
                          DT::dataTableOutput("myTable")
                 )
)

#================
#Server

server <- function(input, output, session) {
  pso = init_pso(100)
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  pso_output = reactive({
    run_pso(input$iter, 100)
  })
  
  output$PlotKMeans <- renderPlot({
    
    req(input$xcol)
    req(input$ycol)
    req(input$clusters)
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  
  output$myTable = DT::renderDataTable({
    iris
  })
  
  output$render_optim = renderText({
    print(pso_output()$g_best)
  })
  
  output$render_particles = renderPlot({
    if (input$iter == pso$iter+1) {
      pso$next_i()
    } else {
      pso <<- pso_output()
    }
    # plot(pso$particles[ ,1], pso$particles[ ,2], xlim=c(-5, 5), ylim=c(-5, 5))
    pso$plot_state()
  })
}


shinyApp(ui, server)
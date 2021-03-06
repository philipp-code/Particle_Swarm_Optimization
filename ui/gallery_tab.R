#====================================================================
# Title: Function Gallery
# Author: Katerina Matysova
#====================================================================

gallery_tab = tabPanel(
  "Function Gallery",
  icon = icon("photo"),
  
  fluidRow(actionButton("gallery_info_button", icon("info"))),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      selectInput(
        inputId = "function_type_select",
        label = "Type of Function:",
        choices = c("Continuous", "Non-Continuous")
      ),
      
      selectInput(
        inputId = "function_select",
        label = "Function:",
        choices = character(0)
        
      ),
      selectInput(
        inputId = "color_select",
        label = "Color:",
        choices = c(
          "Yellow-Red" = "YlOrRd",
          "Yellow-Blue" = "YlGnBu",
          "Darkblue-Yellow" = "viridis",
          "Red-Yellow-Green" = "RdYlGn",
          "Red-Yellow-Blue" = "Spectral"
        )
      ),
      
      sliderInput(
        inputId = "alpha_select",
        label = "Opacity:",
        value = 0.9,
        min = 0.0,
        max = 1.0
      )
      
    ),
    
    mainPanel(fluidRow(
      column(6,
             plotlyOutput("plot_surface") %>% withSpinner(color = "red")),
      column(6,
             plotlyOutput("plot_contour") %>% withSpinner(color = "red"))
      
    ))
  )
)
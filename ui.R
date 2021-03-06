library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders) # use for loading icon
library(shinythemes)

source("ui/introduction_tab.R")
source("ui/gallery_tab.R")
source("ui/visualization_tab.R")
source("ui/comparison_tab.R")

#=========================================================
#========================== UI ===========================
#=========================================================

ui <- fluidPage(
  
  title = tags$head(
    tags$title("Particle Swarm Optimization"),
    tags$link(rel = "shortcut icon", href = "favicon.jpg"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  useShinydashboard(),
  
  chooseSliderSkin("Flat", color = "#e2001a"),
  
  titlePanel(fluidRow(
    column(9, br(), "Particle Swarm Optimization"),
    column(3, img(height = 110, src = "dhbw_logo.png"))
    
  )),
  
  tabBox(
    width = 12,
    introduction_tab,
    gallery_tab,
    visualization_tab,
    comparison_tab
  ),
  
  tags$footer(HTML(

      '© Copyright 2021 <br>
      Katerina Matysova - Frederik Dammeier - Julia Albrecht - Philipp Schneider <br>
      DHBW Ravensburg <br><br>
      <a href="https://github.com/philipp-code/Particle_Swarm_Optimization.git"> Visit the GitHub-Repo</a>
      '
  ),
  align = "center")
  
)
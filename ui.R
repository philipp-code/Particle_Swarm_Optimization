library(plotly)
library(shinyWidgets)
library(shinydashboard) # use for box
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
  title = tags$head(tags$title("Particle Swarm Optimization"), tags$link(rel="shortcut icon", href="favicon.jpg")),
  theme = shinytheme("united"),
  setBackgroundColor("#F5F5F5"),
  
  useShinydashboard(),
  
  tags$style(".small-box.bg-aqua { background-color: #FFFFFF !important; color: #000000 !important; word-wrap: break-word;}"),
  
  titlePanel(fluidRow(
    column(9, br(), "Particle Swarm Optimization"),
    column(3, img(height = 110, src = "dhbw_logo.png"))
  )),
  
  tabBox( width = 12,
    introduction_tab, 
    gallery_tab,
    visualization_tab,
    comparison_tab
  )
)
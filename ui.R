library(shiny)
library(leaflet)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(shinyWidgets)

#--------
# Define UI 
shinyUI(
  
  # Define UI
  fluidPage(  
    setBackgroundColor(color = "#C7DAE0"),
    title="Robomussels",
    titlePanel(
      div(tags$img(src="TrenchEdLogo.png", height = 150), 
          "How Organisms Experience Climate Change")
    ),
    includeHTML("include.html"),
    
    # Select sites and map
    sidebarLayout(
      
      # Define the sidebar with one input
      sidebarPanel(
        checkboxGroupInput("sites", "Sites:", 
                           choices = unique(te.max$location),
                           selected = unique(te.max$location)[1])
      ),
      
      # Create a spot for ggplot
      mainPanel(
        leafletOutput('map'),
      )
    ),
    
    hr(),
    includeMarkdown("include2.md"),
    
    hr(),
    plotOutput("climbPlot", height = "800px"),
      
    hr(),
    includeMarkdown("include3.md"),
    
    # hr(),
    # plotOutput("ampPlot", height = "800px"),
    
    hr(),
    includeMarkdown("include4.md"),
    
    hr(),
    plotOutput("quiltPlot", height = "400px"),
    
    hr(),
    includeMarkdown("include5.md"),
    br(), br()
  )
    
) #end shiny ui
    
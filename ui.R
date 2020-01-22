library(leaflet)

#--------
# Define UI 
shinyUI(
  
  # Define UI
  fluidPage(  
    title = "Robomussel temperatures",
    fluidRow(
      column(12,
             includeMarkdown("include.md")
      )),
    
    # Select sites and map
    fluidRow(
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        checkboxGroupInput("sites", "Sites:", 
                           choices=unique(te.max$location),
                           selected = unique(te.max$location)[1])
      ),
      
      # Create a spot for ggplot
      mainPanel(
        leafletOutput('map') 
      )
      ) #end sidebar
    ),
    
    hr(),
      fluidRow(
        column(12,
               includeMarkdown("include2.md")
        )),
    
    hr(),
    fluidRow(
      mainPanel(
        plotOutput("climbPlot", height="800px")  
      )),
      
      hr(),
      fluidRow(
        column(12,
               includeMarkdown("include3.md")
        )),
    
    hr(),
    fluidRow(
      mainPanel(
        plotOutput("ampPlot", height="800px")  
      )),
    
    hr(),
    fluidRow(
      column(12,
             includeMarkdown("include4.md")
      )),
    
    hr(),
    fluidRow(
      mainPanel(
        plotOutput("quiltPlot", height="400px")  
      )),
    
    hr(),
    fluidRow(
      column(12,
             includeMarkdown("include5.md")
      ))
    )
    
) #end shiny ui
    
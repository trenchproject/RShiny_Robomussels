#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  theseSites <- reactive(subset(te.max, te.max$location %in% c(input$sites)))
  
  points <- reactive(theseSites()[, c("lon", 'lat')])
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircleMarkers(data = points())
  })
  
  #times series plot
  output$climbPlot <- renderPlot({
    
    ggplot(data = theseSites(), aes(x = doy, y = MaxTemp_C, color = subsite)) + 
      geom_line(alpha = 0.8) +
      theme_bw() + 
      guides(color = FALSE) + 
      labs(x = "Day of year", y = "Maximum daily temperature (°C)") + 
      facet_wrap( ~ location, ncol = 1) +
      theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12), axis.title = element_text(size = 13),
            legend.text = element_text(size = 12), legend.title = element_text(size = 12))
    
  })
  
 
  #fourier series plot
  powSites = reactive(subset(pow, pow$location %in% c(input$sites)))
    
   output$ampPlot <- renderPlot({
    
    ggplot(data = powSites(), aes(x = log(freq), y = log(cyc_range / 2))) + 
       geom_line(alpha = 0.8, aes(color = subsite)) + 
       theme_classic() + guides(color = FALSE, size = FALSE) + facet_wrap( ~ location) +
       geom_vline(xintercept = -2.639, color = "gray") + 
       geom_vline(xintercept = -1.946, color = "gray") + 
       geom_vline(xintercept = -3.40, color = "gray") + 
       geom_vline(xintercept = -5.9, color = "gray") + 
       labs(x = "log (frequency) (1/days)", y = "log (amplitude)") + 
       annotate(geom = "text", x = -2.1, y = -5.5, label = "1 week", size = 4, color = "black", angle = 90)+ 
       annotate(geom = "text", x = -2.8, y = -5.5, label = "2 weeks", size = 4, color = "black", angle = 90)+ 
       annotate(geom = "text", x = -3.6, y = -5.5, label = "1 month", size = 4, color = "black", angle = 90)+ 
       annotate(geom = "text", x = -6.2, y = -5.5, label = "1 year", size = 4, color = "black", angle = 90) + 
       ylim(range(-5.8, 1.2)) +
       theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12), axis.title = element_text(size = 13),
             legend.text = element_text(size = 12), legend.title = element_text(size = 12))
    
  })
  
  #quilt plot
  output$quiltPlot <- renderPlot({
    
    ggplot(theseSites() %>% group_by(lat, month) %>% summarise(max = max(MaxTemp_C), 
                                                               mean.max = mean(MaxTemp_C), 
                                                               q75 = quantile(MaxTemp_C, 0.75), 
                                                               q95 = quantile(MaxTemp_C, 0.95))) + 
      aes(x = month, y = as.factor(round(lat, 2))) + 
      geom_tile(aes(fill = mean.max)) + 
      coord_equal() +
      scale_fill_gradientn(colours = rev(heat.colors(10)), name = "Temperature (°C)") +
      #scale_fill_distiller(palette="Spectral", na.value="white", name="max temperature (°C)") + 
      theme_classic(base_size = 18) + 
      xlab("Month") + ylab("Latitude (°)") + 
      theme(legend.position = "bottom") +
      geom_hline(yintercept = 7.5, color = "white", lwd = 2) +
      theme(legend.key.width = unit(5, "line")) +
      scale_x_continuous(breaks = seq(1, 12, 1))
    
  })
  
})

#---------------


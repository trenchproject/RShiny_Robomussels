#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # te.max <- readRDS("tedat.rds")
  # #count by site, subsite, year
  # te.count <- te.max %>% group_by(year,site, subsite) %>% summarise( count=length(MaxTemp_C)  )
  # te.count= as.data.frame(te.count)
  # 
  # #subset sites
  # te.max2 <- subset(te.max, te.max$site %in% c("SD","BB","PD") ) # "HS",
  # #te.max2= subset(te.max, te.max$lat %in% c(48.39137,44.83064,35.66582,34.46717) )
  # 
  # # USWACC	48.5494	-123.0059667	Colins Cove
  # # USWACP	48.45135	-122.9617833	Cattle Point
  # #* USWASD	48.39136667	-124.7383667	Strawberry Point
  # #* USORBB	44.83064	-124.06005	Boiler Bay
  # #* USCAPD	35.66581667	-121.2867167	Piedras
  # # USCAAG	34.46716667	-120.2770333	Alegria
  # 
  # 
  # #time series
  # te.max1<-subset(te.max2, te.max2$year==2002)
  # 
  # #restrict to summer
  # #May 1 through September: 121:273 
  # te.max1<-subset(te.max1, te.max1$doy>120 & te.max1$doy<274)
  
  theseSites = reactive(subset(te.max, te.max$location %in% c(input$sites)))
  
  points = reactive(theseSites()[,c("lon", 'lat')])
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% addCircleMarkers(data = points())
  })
  
  #times series plot
  output$climbPlot <- renderPlot({
    
    ggplot(data=theseSites(), aes(x=doy, y = MaxTemp_C, color=subsite))+geom_line(alpha=0.8) +
           theme_bw() + guides(color=FALSE)+labs(x = "Day of year",y="Maximum daily temperature (°C)") + facet_wrap(~location, ncol = 1)
    
  })
  
 
  #fourier series plot
  powSites = reactive(subset(pow, pow$location %in% c(input$sites)))
    
   output$ampPlot <- renderPlot({
    
    ggplot(data=powSites(), aes(x=log(freq), y = log(cyc_range/2) ))+geom_line(alpha=0.8, aes(color=subsite)) +theme_classic()+ guides(color=FALSE, size=FALSE)+ facet_wrap(~labs)+
      geom_vline(xintercept=-2.639, color="gray")+geom_vline(xintercept=-1.946, color="gray")+geom_vline(xintercept=-3.40, color="gray")+geom_vline(xintercept=-5.9, color="gray")+
      labs(x = "log (frequency) (1/days)",y="log (amplitude)")+
      annotate(geom="text", x=-2.1, y=-5.5, label="1 week", size=3, color="black",angle=90)+ 
      annotate(geom="text", x=-2.8, y=-5.5, label="2 weeks", size=3, color="black",angle=90)+ 
      annotate(geom="text", x=-3.6, y=-5.5, label="1 month", size=3, color="black",angle=90)+ 
      annotate(geom="text", x=-6.2, y=-5.5, label="1 year", size=3, color="black",angle=90)+ylim(range(-5.8,1.2))
    #add lines for 1 week, 2 week, month, year
    
  })
  
  #quilt plot
  output$quiltPlot <- renderPlot({
    
    ggplot(theseSites() %>% group_by(lat, month) %>% summarise( max=max(MaxTemp_C), mean.max=mean(MaxTemp_C), q75= quantile(MaxTemp_C, 0.75), q95= quantile(MaxTemp_C, 0.95) ) 
    ) + 
      aes(x = month, y = as.factor(round(lat,2)) ) + 
      geom_tile(aes(fill = mean.max)) + 
      coord_equal()+
      scale_fill_gradientn(colours = rev(heat.colors(10)), name="temperature (°C)" )+
      #scale_fill_distiller(palette="Spectral", na.value="white", name="max temperature (°C)") + 
      theme_classic(base_size = 18)+xlab("month")+ylab("latitude (°)")+ theme(legend.position="bottom")+ #+ coord_fixed(ratio = 4)
      geom_hline(yintercept = 7.5, color="white", lwd=2) +
      scale_x_continuous(breaks=seq(1,12,2))
  })
  
})

#---------------










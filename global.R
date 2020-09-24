library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(stats)

site.dat = read.csv("https://github.com/ajijohn/ClimateBiology/blob/master/data/musselREADME.csv")
#Load robomussel data
te.max <- readRDS("tedat.rds")

#Load meta for names
te.meta <- read.csv("musselREADME.csv")
te.meta = te.meta %>% mutate(location = paste0(location, ", ", region))
namemap = unique(te.meta[, c("location", "latitude", "longitude")])

ungroup(namemap)
# add location names
te.max = te.max %>% ungroup() %>% left_join(namemap, c('lat' = 'latitude'))
te.max = te.max %>% rename(lon = longitude)

#count by site, subsite, year
te.count <- te.max %>% group_by(year, site, subsite) %>% summarise(count = length(MaxTemp_C))
te.count = as.data.frame(te.count)


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


#time series
te.max <-subset(te.max, te.max$year == 2002)

#restrict to summer
#May 1 through September: 121:273 
te.max <-subset(te.max, te.max$doy > 120 & te.max$doy < 274)

#Load Fourier analysis data
pow <- readRDS("powdat.rds")
#make site a factor
pow$site = as.factor(pow$site)
#add location
locs = as.data.frame(te.max[!duplicated(te.max$site), ])
pow$location = locs[match(pow$site, locs$site), 'location']
pow$location = pow$location

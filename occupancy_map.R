# creates a map of occupancy probability estimates
# case study is on lynx in the French Alps

# a single-season occupancy model was fitted 
# to detection/non-detection data; initial occupancy 
# is a function of forest cover while species detection
# has a site-random effect on it

# see Gimenez, O., L. Blanc, A. Besnard, R. Pradel, P. F. Doherty Jr, E. Marboutin and R. Choquet (2014). 
# Fitting occupancy models with E-SURGE: hidden-Markov modelling of presence-absence data. 
# Methods in Ecology and Evolution. 5: 592–597.
# freely available from: https://dl.dropboxusercontent.com/u/23160641/my-pubs/Gimenezetal2014.pdf

library(rgdal)
library(RColorBrewer)

# read in grid with forest cover
lynx <- readOGR(".", "lynx")
names(lynx@data)

# occupancy estimates from model psi(forest),p(random)
l_psi <- -3.12 + 9.33 * lynx@data$forest # on the logit scale
psi <- 1/(1+exp(-l_psi)) # back-transformed
lynx@data$occ <- as.vector(psi) # store in the shp

# get quantiles of occupancy estimates
grid_occ <- quantile(lynx@data$occ,probs=seq(0, 1, 0.11))
round(grid_occ,2)

# get quantiles of forest cover
grid_forest <- quantile(lynx@data$forest,probs=seq(0, 1, 0.11))
grid_forest

# palette of greys (9 levels)
cols <- brewer.pal(n=9,name="Greys") 

# map forest cover and occupancy estimates side by side
par(mfrow=c(1,2))

# forest cover first
lcols_f <- cut(lynx@data$forest,breaks= grid_forest,labels=cols) # discretize forest cover using quantiles
plot(lynx, axes=TRUE,border="gray",main='forest cover') # background grid
plot(lynx, col=as.character(lcols_f), add=TRUE) # add forest cover

# occupancy estimates second
lcols_psi <- cut(lynx@data$occ,breaks= grid_occ,labels=cols) # discretize occupancy using quantiles
plot(lynx, axes=TRUE,border="gray",main='occupancy') # background grid
plot(lynx, col=as.character(lcols_psi), add=TRUE) # add occupancy estimates

# add legend
text_legend = c('0.04-0.10','0.10-0.22','0.22-0.39','0.39-0.54','0.54-0.74','0.74-0.83','0.83-0.90','0.90-0.98','0.98-1.00')
legend(940500, 2250000,legend= text_legend,col=cols,pch=15)

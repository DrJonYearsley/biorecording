# A script to calculate the alpha values for Frescalo 
# The estimation of trends in species occurence has not been implemented
#
# This has been checked against Frescalo output and it agrees
#
# Written by Jon Yearsley (jon.yearsley@ucd.ie) 2nd Sept 2016

setwd('/home/jon/MEGA/Jack/Frescalo')
rm(list=ls())  # Remove all variables from the memory

require(foreach, quietly=T)
require(doParallel, quietly=T)

# Set the size of the cluster (number of nodes). 
# cores=NULL will automatically pick the number of cores per node 
# (by defauult this is half the number of available cores)
cl <- makeCluster(2)
registerDoParallel(cl, cores=2)


weight.file = './bsbi_2009_weight.txt'
species.file = './bsbi_2009_sample.txt'

Phi = 0.74    # The standardisation for recorder effort

# Import data
d = read.table(weight.file, header=F, col.names=c('Target','Hectad','w','w1','w2','nsim','ndist'), stringsAsFactors=F)
s = read.table(species.file, header=F, col.name=c('Hectad','Species','Year'), stringsAsFactors=F)


##########################
# Function definitions

min.fun <- function(alpha,f,Phi) {
  # The function to minimise to fit Frescalo model to the data 
  # Page 5 column 2 in Hill (2011)
  F=1-(1-f)^alpha
  F[abs(f-1)<1e-10] = 1
  return( sum(F^2)/sum(F) - Phi)
}

frescalo = function(focal_d, speciesList, spLocations) {
# This function calculates the sampling effort multiplier, alpha, 
# that would equate sampling effort across all regions
# This is the main method of Frescalo.
  
  focal = focal_d$Target[1]
  
  # Identify species in neighbourhood of focal region 
  speciesRegional = speciesList[match(focal_d$Hectad, spLocations)]
  
  #    speciesRegional = speciesList[match(as.character(d$Hectad), targets)]
  # Calculate weights of locations in the neighbourhood
  weights = focal_d$w/sum(focal_d$w)

  # Create weighted neighbourhood frequencies (checked against Frescalo)
  frequency = Reduce('+',Map('*',as.list(weights), speciesRegional))  
  
  phi_in = sum(frequency^2) / sum(frequency)
  
  # Calculate the multiplier (alpha) that equalises recording effort 
  alpha.max = 5
  # Increase alpha.max until min.fun() becomes positive (i.e. ensure there is a zero)
  while (min.fun(alpha.max, frequency, Phi)<0) { alpha.max = alpha.max + 5}  

  # Find sampling-effort multiplier
  sol=uniroot(min.fun,interval=c(0.01,alpha.max), tol=0.001, frequency, Phi)
  alpha = sol$root

  return(data.frame(location=focal, alpha=sol$root, phi_in=phi_in, iter=sol$iter))
}

#######################################################

grouping = as.factor(s$Hectad)
spLocations = levels(grouping)

species = as.character(unique(s$Species))   # Create list of unique species

# Limit focal regions to ones where we have species data
dSub = subset(d, Target %in% spLocations)
targets = as.character(unique(dSub$Target))    # Create list of unique focal regions

# For each region record presence/absence of each species (a_ij in Hill 2011)
sSplit = split(s, grouping)  # Split species data up into hectads

idx = iter(sSplit)
speciesList <- foreach(spList = idx, .inorder=T) %do% {
  as.integer(species %in% spList$Species)
}

# For each focal regional calculate the sampling effort multipler
dSplit = split(dSub[,1:3], as.factor(dSub$Target))  # Split data up into focal regions
idx2 = iter(dSplit)
frescalo.out <- foreach(focal_data = idx2, .inorder=T, .combine='rbind') %dopar% { 
  frescalo(focal_data, speciesList, spLocations)
}


stopCluster(cl)
gc()
###################################################################################################
# Functions and Setup
###################################################################################################
library(maptools)
library(raster)

species = 'Paspalum urvillei'
#Set where yout maxent jar file is
maxent.location = 'C:/Users/Vernon/Dropbox/Work/SDMs/maxent/maxent.jar'
#Read in one environmental layer for data cleaning purposes etc.
bioRaster = raster('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/WORLDCLIM/10_min/bio/wc2.0_bio_10m_01.tif')

#Normalization function; ensures predicted probabilities sum to 1. 
norm = function(surf){ surf/sum(surf,na.rm=T)}
#Automatically create places to put maxent output
model.output = 'Output/Maxent_output' 
if(!file.exists(model.output)) {dir.create(model.output)}

# ###################################################################################################
# # Get occurrence data
# ###################################################################################################
# #Use only the occurrences from SAPIA and SANBI, then download the rest from GBIF, as per below
# occ = read.csv('Data/Occurrences_Alien_Paspalums.csv')
# occ = occ[!(occ$database=='GBIF'),]
# 
# levels(occ$acceptName)
# paspalumSpp = c("Paspalum dilatatum","Paspalum notatum","Paspalum nutans","Paspalum quadrifarium","Paspalum urvillei",
#                 "Paspalum virgatum")
# paspalumSpEpithet = sapply(paspalumSpp, function(x) strsplit(x, split=' '))
# paspalumSpEpithet = unname(unlist(lapply(paspalumSpEpithet, '[', 2)))
# 
# #Get species occurrences from GBIF
# library(rgbif)
# #Check GBIF is getting the correct species names
# for (s in 1:length(paspalumSpp)){
#   (spNames = head(name_lookup(query=paspalumSpp[s], rank="species", return = 'data')))
#   print(as.data.frame(spNames[1,2]))
# }
# 
# #Get data from GBIF for each species and add to occurrence database
# for (s in 1:length(paspalumSpp)){
#   (spNames = head(name_lookup(query=paspalumSpp[s], rank="species", return = 'data')))
#   spDatGBIF = occ_search(scientificName = paspalumSpp[s])
#   spDat = as.data.frame(spDatGBIF$data)
#   assign(paste0('spDat_',paspalumSpEpithet[s]), spDat)
#   occSp = data.frame(origName=spDat$name,
#                          decimalLongitude=spDat$decimalLongitude,
#                          decimalLatitude=spDat$decimalLatitude,
#                          year=spDat$year,
#                          database='GBIF',
#                          acceptName=spDat$name)
#   occ = rbind(occ, occSp)
# }
# 
# #Change some species names
# levels(occ$acceptName)
# occ$acceptName = as.character(occ$acceptName)
# occ$acceptName[occ$acceptName%in%c("Paspalum notatum Fl?gg?","Paspalum notatum Flugge.")] = "Paspalum notatum"
# occ$acceptName[occ$acceptName=="Paspalum nutansï¿½Lam."] = "Paspalum nutans Lam."
# occ$acceptName[occ$acceptName=="Paspalum dilatatum Poir."] = "Paspalum dilatatum"
# occ$acceptName[occ$acceptName=="Paspalum quadrifarium Lam."] = "Paspalum quadrifarium"
# occ$acceptName[occ$acceptName=="Paspalum urvillei Steud."] = "Paspalum urvillei"
# occ$acceptName[occ$acceptName=="Paspalum virgatum L."] = "Paspalum virgatum"
# occ$acceptName = factor(occ$acceptName)
# occ$name = occ$acceptName
# levels(occ$acceptName)
# 
# #Add an ID field:
# occ$ID = 1:nrow(occ)
# 
# write.csv(occ, 'Output/occ.csv', row.names=F)
# 
# ###################################################################################################
# # DATA CLEANING
# ###################################################################################################
# #Read in occurrence data
# occ = read.csv('Output/occ.csv')
# 
# #View occurrence data
# library(mapr)
# occ$longitude=occ$decimalLongitude
# occ$latitude=occ$decimalLatitude
# map_ggplot(data.frame(name=occ$name, longitude=occ$decimalLongitude, latitude=occ$decimalLatitude))
# 
# #Remove spurious points
# occ = occ[occ$decimalLongitude!=0 & occ$decimalLatitude!=0,] #Remove points at 0,0
# summary(occ$decimalLongitude)
# summary(occ$decimalLatitude)
# occ = occ[-unique(which(is.na(occ$decimalLongitude)|is.na(occ$decimalLatitude), arr.ind = T)), ] # Removing NA coordinates
# 
# #Add fields required by biogeo:
# library(biogeo)
# checkdatastr(occ)
# names(occ)
# occBiogeo = renamefields(occ, x='decimalLongitude', y='decimalLatitude', Species='name')
# occBiogeo = addmainfields(occBiogeo, species='Species')
# head(occBiogeo)
# 
# #Move points on land to nearest cell in sea:
# occBiogeo = nearestcell(occBiogeo, bioRaster)
# #Check if any points were moved
# occBiogeo$moved
# map_ggplot(data.frame(name=as.character(occBiogeo$moved$ID), longitude=occBiogeo$moved$x, latitude=occBiogeo$moved$y))
# #Change to coordinates on land:
# occ$decimalLongitude[occ$ID%in%occBiogeo$moved$ID] = occBiogeo$moved$x
# occ$decimalLatitude[occ$ID%in%occBiogeo$moved$ID] = occBiogeo$moved$y
# 
# write.csv(occ, 'Output/occ.csv', row.names=F)
# 
# ###################################################################################################
# # EXTENT SIZE
# ###################################################################################################
# occ = read.csv('Output/occ.csv')
# 
# cellIDs = cellFromXY(bioRaster, cbind(occ$decimalLongitude,occ$decimalLatitude))
# occNonDup = occ[!duplicated(cellIDs),]
# 
# rastOcc = bioRaster
# rastOcc[] = NA
# occCells = cellFromXY(rastOcc, cbind(occ$decimalLongitude,occ$decimalLatitude))
# occCells = occCells[!is.na(occCells)]
# rastOcc[occCells] = 1
# buf = buffer(rastOcc, width=4.5) #Buffer width =~500 km
# plot(buf)
# 
# #https://rossijeanpierre.wordpress.com/2014/01/31/extracting-circular-buffers-from-a-raster-in-r-12/
# library(rgeos)
# rast = bioRaster
# rast[]= 1
# occPoints = SpatialPoints(cbind(occNonDup$decimalLongitude,occNonDup$decimalLatitude))
# projection(occPoints) = projection(rast)
# occBuffer = gBuffer(occPoints, width=4.5)
# bufferRasterize = rasterize(occBuffer, rast) # rasterize the generated buffer
# bufferMask = mask(x=rast, mask=bufferRasterize) # Create your mask by putting NA in all cells outside the boundary
# plot(bufferMask)
# writeRaster(bufferMask, filename='Output/bufferMask.tif', format='GTiff', overwrite=T)
# 
# ###################################################################################################
# # VARIABLE SELECTION
# ###################################################################################################
# #Load occurrence data:
# occ = read.csv('Output/occ.csv')
# 
# #Load buffer mask
# bufferMask = raster('Output/bufferMask.tif')
# 
# #Load bioclim rasters
# bioFiles = list.files('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/WORLDCLIM/10_min/bio/')
# bioRasters = raster::stack(paste0('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/WORLDCLIM/10_min/bio/',
#                                   bioFiles))
# names(bioRasters)
# #See http://worldclim.org/bioclim for description of bioclim vars
# 
# #Select only ecologically relevant variables
# ecoRevRasters = dropLayer(bioRasters, c(2,3,5,6,7,8,9,10,11,13,16,17,18,19))
# 
# #Get road density raster
# roadDens = raster('Output/Global_5minAggregated_NoMask_gROADS-v1.tif')
# roadDens = aggregate(roadDens, fact=2, fun=mean)
# names(roadDens) = 'roadDens'
# 
# #Get population density raster:
# popDens = raster('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Human_impacts/SEDAC_pop_density/gpw-v4-population-density_2010.tif')
# res(ecoRevRasters)/res(popDens)
# popDens = aggregate(popDens, fact=20)
# names(popDens) = 'popDens'
# 
# #Crop all rasters to same extent
# extent(popDens)
# extent(roadDens)
# extent(ecoRevRasters)
# ecoRevRasters = crop(ecoRevRasters, extent(popDens))
# roadDens = crop(roadDens, extent(popDens))
# bufferMask = crop(bufferMask, extent(popDens))
# 
# #Mask by buffer raster
# ecoRevRasters = mask(ecoRevRasters, bufferMask)
# popDens = mask(popDens, bufferMask)
# roadDens = mask(roadDens, bufferMask)
# 
# #See correlation among predictors (the first time you do this you will need to install ENMTools)
# # library(devtools)
# # install_github("danlwarren/ENMTools")
# library(ENMTools)
# raster.cor.matrix(ecoRevRasters)
# raster.cor.plot(ecoRevRasters)
# 
# #Write Bio rasters to ASCII format for Maxent
# if(!file.exists('Output/RastersEnv/')) {dir.create('Output/RastersEnv/')}
# for(r in 1:nlayers(ecoRevRasters)){
#   writeRaster(ecoRevRasters[[r]], paste0('Output/RastersEnv/',names(ecoRevRasters)[r],'.asc'), format='ascii', overwrite=T)
# }
# writeRaster(popDens, 'Output/RastersBias/popDens.asc', format='ascii', overwrite=T)
# writeRaster(roadDens, 'Output/RastersBias/roadDens.asc', format='ascii', overwrite=T)

###############################################################################
# Get occurrence data ready for modelling
###############################################################################
#Specify output location
if(!file.exists('Output/OccurrenceData')) {dir.create('Output/OccurrenceData')}

#Read in cleaned occurrence data:
occ = read.csv('Output/occ.csv')
occSp = occ[occ$name==species,c('name','decimalLongitude','decimalLatitude')]
occBack = occ
occBack = occBack[,c('name','decimalLongitude','decimalLatitude')]
occBack$name = 'bias'
spFileName = paste(strsplit(species, split=' ')[[1]], collapse='_')
write.csv(occSp, paste0('Output/OccurrenceData/occ',spFileName,'.csv'), row.names=F)
write.csv(occBack, 'Output/OccurrenceData/occBack.csv', row.names=F)

###############################################################################
# Sampling bias model
###############################################################################
#Specify some Maxent settings
biasSettings = paste0('java -jar ',maxent.location, ' nowarnings noprefixes responsecurves jackknife outputformat=raw noaskoverwrite -a -z threads=3 replicates=5 nothreshold nohinge noautofeature noremoveduplicates ')
                            
#nowarnings = don't output warnings when running Maxent
#noprefixes = 
#responsecurves = provide plots of response curves 
#jackknife = do jackknife predictor importance calculation
#outputformat=raw = provide raw Maxent output format and not logistic format
#noaskoverwrite = don't ask whether to overwrite existing files
#-a =
#-z =
#threads=3 = use up to 3 memory threads
#replicates=5 = run occurrence and background selection 5 times
#nothreshold nohinge noautofeature = don't use threshold, hinge and auto-selection of features
#noremoveduplicates = don't remove duplicate occurrences within grid cells

#Specify output location
output = paste0(model.output,'/TGBias')
if(!file.exists(output)) {dir.create(output)}

#Provide predictor variables location (make sure that the correct variables are in this folder)
predictorsLoc = paste0('Output/RastersBias/')

#Give location of occurrences
occurrencesLoc = 'Output/OccurrenceData/occBack.csv'

#Run model
system(paste0(biasSettings,
              ' outputdirectory=',output,
              ' environmentallayers=',predictorsLoc,
              ' samplesfile=',occurrencesLoc))

################################################################################
## Species models
################################################################################
#=========================================================================
#Run MaxEnt for species of interest with no sampling bias
#=========================================================================
#Specify some Maxent settings (same as for the sampling bias model, but here we remove duplicates)
maxentNoBiasSettings = paste0('java -jar ',maxent.location, ' nowarnings noprefixes responsecurves jackknife outputformat=raw noaskoverwrite -a -z threads=3 replicates=5 nothreshold nohinge noautofeature removeduplicates ')

#Specify output folder
output = paste0(model.output,'/',spFileName,'MaxentNoBias')
if(!file.exists(output)) {dir.create(output)}

#Provide predictor variables location (make sure that the correct variables are in this folder)
predictorsLoc = paste0('Output/RastersEnv/')

#Give location of occurrences
occurrencesLoc = paste0('Output/OccurrenceData/occ',spFileName,'.csv')

#Run model
system(paste0(maxentNoBiasSettings,
              'outputdirectory=',output,
              ' environmentallayers=',predictorsLoc,
              ' samplesfile=',occurrencesLoc))

#=========================================================================
#Run MaxEnt for species of interest WITH sampling bias
#=========================================================================
#Specify some Maxent settings (same as for the sampling bias model, but here we remove duplicates PLUS note the biastype=3)
maxentBiasSettings = paste0('java -jar ',maxent.location, ' nowarnings noprefixes responsecurves jackknife outputformat=raw noaskoverwrite -a -z threads=3 replicates=5 nothreshold nohinge noautofeature removeduplicates biastype=3 ')

#Specify output folder
output = paste0(model.output,'/',spFileName,'MaxentBias')
if(!file.exists(output)) {dir.create(output)}

#Provide predictor variables location (make sure that the correct variables are in this folder)
predictorsLoc = paste0('Output/RastersEnv/')

#Give location of occurrences
occurrencesLoc = paste0('Output/OccurrenceData/occ',spFileName,'.csv')

#Get sampling bias raster
bias = paste0(model.output,'/TGBias/bias_avg.asc')
biasRast = raster(bias)

#Run model
system(paste0(maxentBiasSettings,
              'outputdirectory=',output,
              ' environmentallayers=',predictorsLoc,
              ' samplesfile=',occurrencesLoc,
              ' biasfile=',bias))

################################################################################
## Show output
################################################################################
#Read in all the models
#Maxent model with no sampling bias model included
maxent.pred = raster(paste0('Output/Maxent_output/',spFileName,'MaxentNoBias/',spFileName,'_avg.asc')) 
names(maxent.pred) = c('maxent.pred')
#Maxent model with sampling bias model included
sampling.minxent.pred = raster(paste0('Output/Maxent_output/',spFileName,'MaxentBias/',spFileName,'_avg.asc'))
names(sampling.minxent.pred) = c('sampling.minxent.pred')
#TG sampling bias model 
sampling.prior = raster('Output/Maxent_output/TGBias/bias_avg.asc')
names(sampling.prior) = c('sampling.prior')

#Stack together
allMods = stack(maxent.pred, sampling.minxent.pred, sampling.prior)

#Normalise values to make comparison across models possible
values(allMods) = apply(values(allMods),2,norm)

#Plot models
plot(allMods)
plot(sampling.minxent.pred)

saBorders = shapefile('Data/GIS/South_Africa.shp')
sampling.minxent.pred.SA = crop(sampling.minxent.pred, extent(saBorders))
sampling.minxent.pred.SA = mask(sampling.minxent.pred.SA, saBorders)
plot(sampling.minxent.pred.SA)

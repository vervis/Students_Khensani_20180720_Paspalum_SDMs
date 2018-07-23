setwd('C:/Users/Vernon/Dropbox/Work/Teaching/2016 Khensani/SDMs Paspalum/')


library(raster)
bioFiles = list.files('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/WORLDCLIM/30_sec/bio')
bioRasters = raster::stack(paste0('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/WORLDCLIM/30_sec/bio/',
                                 bioFiles))


koppen = raster('C:/Users/Vernon/Dropbox/GIS/Earth_observations/Climate/CliMond/Koppen_Geiger/10_min/CM10_1975H_Kop_V1.1/koppen')



https://rdrr.io/cran/virtualspecies/man/removeCollinearity.html
http://scottsfarley.com/research/sdm/2016/05/20/Selecting-Variables-for-SDM.html


https://groups.google.com/forum/#!topic/maxent/NtKTyF9v8v4


#Get occurrence data:
aliensSA = read.csv('Occurrence data/Alien and extra-limital grass distributions SA.csv')
aliensSA = aliensSA[aliensSA$database!='GBIF',] #Remove occurrences from GBIF
presSA = aliensSA[grep('Paspalum dilatatum', aliensSA$acceptName),] #Select only species of interest
presGBIF = read.csv('Occurrence data/GBIF.P.dilatatum.0031537-180508205500799.csv', sep='\t') #Get GBIF data

back = read.csv('C:/Users/Vernon/Dropbox/Work/Postdoc_CIB/Research/Phalaris/Phalaris_all_species/Distribution_data/distDat_arundinacea.csv')


#Check species names. If there are obvious names that you know shouldn't be this species, then remove them
levels(presGBIF$scientificname)

#Combine SA data with GBIF data:
names(presSA)
names(presGBIF)
pres = presSA[,c('x','y','year')]
pres = rbind(pres, data.frame(x=presGBIF$decimallongitude, y=presGBIF$decimallatitude, year=presGBIF$year))
pres$ID = 1:nrow(pres)
pres$Species = 'Paspalum dilatatum'

#View occurrence data
library(maptools)
data("wrld_simpl")
plot(wrld_simpl)
points(pres$x, pres$y, col='red')

#Get frequency of points in grid cells and identify and remove duplicate presences:
rst = bioRasters[[1]]
rst[] = 1:ncell(rst)
xyP = data.frame(pres$x, pres$y)
cidP = cellFromXY(rst, xyP)
summary(duplicated(cidP))
pres = pres[!duplicated(cidP),]

#Add fields required by biogeo:
library(biogeo)
checkdatastr(pres)
pres = addmainfields(pres, species = 'Species')
head(pres)

#Move points on land to nearest cell in sea:
presx = nearestcell(pres, bioRasters[[1]])
pres$x_land = pres$x
pres$y_land = pres$y
pres$x_land[pres$ID%in%presx$moved$ID] = presx$moved$x
pres$y_land[pres$ID%in%presx$moved$ID] = presx$moved$y

#Check precision of record coordinates and remove imprecise records:
p10min = precisioncheck(dat=pres, x='x', y='y', s=10, e=60)
pres$preci = 0
pres$preci[p10min$preci==1] = 1
p = precisioncheck(dat=pres, x='x', y='y', s=0.5, e=0.5)
pres$preci[p$p0.5m==1] = 1
pres = pres[pres$preci==0,]
b10min = precisioncheck(dat=back, x='x', y='y', s=10, e=10)
back$preci10min = 0
back$preci10min[b10min$p10m==1] = 1
b = precisioncheck(dat=back, x='x', y='y', s=0.5, e=0.5)
back$preci = 0
back$preci[b$p0.5m==1] = 1
back = back[back$preci==0,]

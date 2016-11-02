# Load Halpern's (2015 Nature comms) global pressures maps and find out how many stressors are in each grid cell
# Original data from here -  https://knb.ecoinformatics.org/#view/doi:10.5063/F19Z92TW
# Paper: Halpern, Benjamin S., et al. "Spatial and temporal changes in cumulative human impacts on the world/'s ocean." Nature communications 6 (2015).


#CJ Brown Oct 2015
# In support of Cote, Darling, Brown in prep. 

rm(list = ls())

library(raster)

setwd('~/Databases/Global stressors/urn_uuid_975e3a96_c912_4e41_a888_7cccab216bf6/data/')

foldnams <- list.files()
nfold <- length(foldnams)

pquant <- 0.1 #threshold for quantile above which we count a stressor

sover <- function(x,y){ return(x+y) }

savenam <- paste('~/Databases/Global stressors/num_stressors_quant', round(pquant*100), '.grd', sep ='')

# Loop through the pressure files

for (ifold in 1:nfold){
	fnam <- list.files(foldnams[ifold])
	print(fnam[1])
	ifile <- grep('.tif', fnam)
	r <- raster(paste(foldnams[ifold],fnam[ifile[1]], sep ='/'))
	
	#Create base raster that will store all values
	if (ifold ==1){
		rsum <- raster(r)
		rsum[] <- 0
		} 
	
	quant <- quantile(r, probs = pquant, type = 7,names = FALSE)
	
	# Reclassify values as 0 if below quantile and 1 otherwise
	rmax <- r@data@max
	rmin <- r@data@min
	rcl <- matrix(c(rmin, quant[1], 0, quant[1], rmax, 1), nrow = 2, byrow = T)
	r2 <- reclassify(r, rcl)
	rm(r)
	
	#Add values to new raster
	# rsum2 <- rsum + r2
	rsum <- overlay(rsum, r2, fun = sover)
	
	# Manually delete temporary files for r2 (the reclassification, each one is >2 gb, so needs to be removed unless you have a lot of harddrive space)
	fdel <- filename(r2)
	fdel2 <- strsplit(fdel, split = '[.]')
	file.remove(fdel)
	file.remove(paste(fdel2[[1]][1], '.gri', sep =''))
	rm(r2)
	
	#Get temporary file name for rsum to delete on next iteration
	if(ifold > 1){ #delete old file
		fdel2 <- strsplit(fdelrsum, split = '[.]')
		file.remove(fdelrsum)
		file.remove(paste(fdel2[[1]][1], '.gri', sep =''))
		}
		
	#Update file name for deleting on next iteration
	fdelrsum <- filename(rsum)

	gc()
	
	}


#Plot to check it
plot(rsum, maxpixels = 10000)

#
# Save to file
#


 writeRaster(rsum, savenam, overwrite = T)

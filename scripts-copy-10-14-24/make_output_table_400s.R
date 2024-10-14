library(lidR)
library(data.table)
library(sf)
library(oce)
## load each file in the directory
## get tree diameter
## get trees with > 1m2 area -- later
## make table 
#files <-  c("TEAK_043")
#c("TEAK_043",'TEAK_044','TEAK_045','TEAK_046','TEAK_047')
#plotIDs <-	c('TEAK_044','TEAK_045','TEAK_046','TEAK_047')#unique(csv$plotID_x)

filenames <- Sys.glob("las_outputs/*.las")

extract_plot_id_num <- function(fpath){

file_isolated <- strsplit(fpath, '/')[[1]][2]

no_extension <- strsplit(file_isolated, '\\.')[[1]][1]

particles <- strsplit(no_extension, '_')

plotID <- paste(particles[[1]][2], particles[[1]][3], sep='_')

plotNum <- particles[[1]][3]

return(list(plotID, plotNum))
}



df <- data.frame()

for (file in filenames){
	#load and drop points that aren't part of a tree
	#fname <- paste0("las_outputs/final_",file,".las.las")	
	plotinfo <- extract_plot_id_num(file)
	plotID <- plotinfo[[1]]
	plotNum <- plotinfo[[2]]
	print(plotID)
	las <- readLAS(file)
	las <- filter_poi(las, ID != 99999)
	dt <- as.data.table(las@data)

	basal_height <- dt[, min(Z), by = ID]
	height <- dt[, max(Z), by = ID]
	
	cm <- crown_metrics(las, geom = "concave", func = NULL, attribute = "ID")
	cm$area <- st_area(cm)
	cm$radius <- sqrt(cm$area/pi)
	cm$cent <- st_centroid(cm$geometry)
	
	
	area_filtered <- cm[as.numeric(cm$area) > 1,,]

	height <- height[ID %in% area_filtered$ID,,]
	basal_height <- basal_height[ID %in% area_filtered$ID,,]
	cm <- area_filtered
	
	
	for(tree in 1:nrow(cm)){
		print(tree)
		coords <- st_coordinates(cm$cent[tree])
		x <- coords[1]
		y <- coords[2]
		latlon <- utm2lonlat(x,y, zone = 11, hemisphere = "N", km = FALSE)
		bh <- basal_height[ID == cm$ID[tree],,]
		h <- height[ID == cm$ID[tree],,]
		uid <- paste0(plotNum,cm$ID[tree])
		#print(uid)
		row <- c(cm$ID[tree], plotID, uid, 400, cm$radius[tree], bh$V1, h$V1,latlon$latitude, latlon$longitude)
		#print(row)
		df <- rbind(df, row)
	}

	names(df) <- c('ID', 'Plot ID', 'UID', 'Plot Area (m2)', 'Radius (m)', 'Base Crown Height (m)', 'Max Crown Height (m)', 'Latitude', 'Longitude')
	print(df)

	#what's next: 

	# run it on all the sites
	# add logic 
	# add uid	
}


	write.csv(df, 'teak_results.csv')

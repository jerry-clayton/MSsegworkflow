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
setDTthreads(0)
filenames <- Sys.glob("sq_km_outs/*.las")
#options(max.print = 100)
extract_plot_id_num <- function(fpath){

file_isolated <- strsplit(fpath, '/')[[1]][2]

no_extension <- strsplit(file_isolated, '\\.')[[1]][1]

particles <- strsplit(no_extension, '_')

plotID <- paste(particles[[1]][1], particles[[1]][3], sep='_')

plotNum <- particles[[1]][3]

return(list(plotID, plotNum))
}


NROW <- 1000000

#names(df) <- c('ID', 'Plot ID', 'UID', 'Plot Area (m2)', 'Radius (m)', 'Base Crown Height (m)', 'Max Crown Height (m)', 'Latitude', 'Longitude')
#df <- data.frame(ID = rep(NA,NROW), plotID = rep(NA, NROW), UID = rep("",NROW),plotarea = rep(NA, NROW), radius = rep(NA,NROW),basecrown = rep(NA,NROW), maxcrown = rep(NA,NROW), lat = rep(NA,NROW),lon = rep(NA,NROW))

#names(df) <- c('ID', 'Plot ID', 'UID', 'Plot Area (m2)', 'Radius (m)', 'Base Crown Height (m)', 'Max Crown Height (m)', 'Latitude', 'Longitude')
trow <- 1	

final_dt <- data.table()
for (file in filenames){
	#load and drop points that aren't part of a tree
	#fname <- paste0("las_outputs/final_",file,".las.las")	
	plotinfo <- extract_plot_id_num(file)
	plotID <- plotinfo[[1]]
	plotNum <- plotinfo[[2]]
	print(plotID)
	print(plotNum)
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
	coords <- st_coordinates(cm$cent)
	print(nrow(coords))
	x <- coords[,1]
	y <- coords[,2]
	#print(x)
	#print(coords[,1])
	latlon <- utm2lonlat(x,y, zone = 11, hemisphere = "N", km = FALSE)
	#print(latlon)
	latlon <- as.data.table(latlon)
	basal_height <- as.data.table(basal_height)
	cm <- as.data.table(cm)
	height <- as.data.table(height)
	uid <- paste0(as.character(plotNum),'_',cm$ID)
	print(uid)
	print(nrow(cm))

	dt <- data.table(cm$ID, rep(plotID,nrow(cm)),uid, rep(1000000, nrow(cm)),cm$radius,basal_height$V1,height$V1,latlon$latitude,latlon$longitude)
	names(dt) <- c('ID', 'Plot ID', 'UID', 'Plot Area (m2)', 'Radius (m)', 'Base Crown Height (m)', 'Max Crown Height (m)', 'Latitude', 'Longitude')
	#fwrite(dt, "test.csv")

#	for(tree in 1:nrow(cm)){
#		if(tree %% 100 == 0){
#			print(tree)
#		}
#		bh <- basal_height[ID == cm$ID[tree],,]
#		h <- height[ID == cm$ID[tree],,]
#		#print(uid)
#		row <- list(cm$ID[tree], plotID, uid, 1000000, cm$radius[tree], bh$V1, h$V1,latlon$latitude[tree], latlon$longitude[tree])
#		#print(row)
#		df[trow,] <-  row
#		trow <- trow + 1
#	}

#	print(df)

	#what's next: 

	# run it on all the sites
	# add logic 
	# add uid
final_dt <- rbindlist(list(final_dt,dt))
print(nrow(final_dt))
}

	#final_dt <- as.data.table(df)
	fwrite(final_dt, 'teak_results.csv')

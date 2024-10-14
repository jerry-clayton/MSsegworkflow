

###
# Set parameters for MS segmentation here
#
###

library(lidR)
library(MeanShiftR)
library(data.table)
library(sf)
library(pracma)

extract_filename <- function(filepath){
	
	return(strsplit(filepath, '/')[[1]][3])

}

# use all available processor cores
set_lidr_threads(0)


#set max angle for merging
angleThreshold <- 70

#change line to switch between 400, 100, and large las's
#filenames <- Sys.glob("las_inputs/*.las")

#filenames <- Sys.glob("las_inputs/normalized/TEAK_1km_norm.las")
#filenames <- Sys.glob("las_outputs/*.las")
#filenames <- "../las_outputs/400_las_outs/final_TEAK_043.las.las"
#filenames <- "../las_outputs/norm_TEAK_043_lidar_2021.las.las"
#filenames <- "../las_outputs/norm_TEAK_043_lidar_2021.las.las"
filecount <- 0

skipped <- list()
#print("enterloop")
#print(filenames)

#get the angle between two tree centroids with reference to the Z axis
#pts should be passed as XYZ coordinates, not SF objs
getAngle <- function(pt1,pt2){
	
	#reference vector for X axis
	ref = c(1,0,0)

	v <- pt2-pt1
	
	# dot product of the two vectors (%*% is dot product operator)
	dotp <- v %*% ref
	
	#magnitude of the vector between two points
	mag_v <- sqrt(sum(v^2))

	#magnitude of reference vector is 1
	mag_r <- sqrt(sum(ref^2))
	
	#compute angle in radians
	#the * 1 is just here to make the formula explicit, obviously unneccesary
	angle_rads <- acos(dotp/(mag_v * mag_r))

	#convert radians to degrees
	angle_deg <- angle_rads * (180/pi)

	return(angle_deg)
}


compute_min_centroid_dist <- function(cm){
	  # Initialize a vector to store the minimum distance for each centroid
	  centroids <- cm
	  min_distances <- numeric(length(centroids))

  

  # Loop through each centroid and compute distances to all other centroids

  for (i in 1:length(centroids)) {

	      # Get the centroid at index i

	      current_centroid <- centroids[i, ]

      

      # Compute distances from current centroid to all other centroids

      distances <- st_distance(current_centroid, centroids)

          

          # Set the distance to itself (index i) to a very large number to exclude it

          distances[i] <- Inf

          

          # Get the minimum distance to another centroid

          min_distances[i] <- min(distances)

	    }
  return(min_distances)
}
h2cw_ratio <- function(height){
	return(10^(-0.1*(height^0.61)))
}

RMSE <- 1.1231

# gets the first neighbor inside the regression radius based on the X-Y centroid.
# in the future should probably evaluate candidates based on Z centroid too.

getMergeID <- function(treeID, centroid, regRadius, cm){
	returnID <- NULL
	for(tree in cm$ID){
		if(tree == treeID){
		## do not test itself
		  next
		}

		tree <- cm[ID == tree,,]
		distance <- as.numeric(st_distance(centroid, tree$centroid.geometry))
		if(distance < regRadius){
			returnID <- tree$ID
			break
		}
	}
	return(returnID)
	
}

# gets the first neighbor inside the regression radius based on the X-Y centroid and XYZ centroid too.

getMergeIDwithAngle <- function(treeID, xycentroid, data_table, regRadius, cm){
	returnID <- NULL
	for(tree in cm$ID){
		tree3dcentroid <- compute_3d_centroids(data_table[ID == treeID,,])
		tree3dcoords <- as.data.table(st_coordinates(tree3dcentroid))
		tree3dcoords <- c(tree3dcoords$X, tree3dcoords$Y, tree3dcoords$Z)
		if(tree == treeID){
		## do not test itself
		  next
		}

		tree <- cm[ID == tree,,]
		distance <- as.numeric(st_distance(xycentroid, tree$centroid.geometry))
		if(distance < regRadius){
			candidate3dcentroid <- compute_3d_centroids(data_table[ID == tree$ID,,])
			cand3dcoords <- as.data.table(st_coordinates(candidate3dcentroid))
			cand3dcoords <- c(cand3dcoords$X, cand3dcoords$Y, cand3dcoords$Z)
			angle <- getAngle(tree3dcoords,cand3dcoords)
			if(angle < angleThreshold){
				returnID <- tree$ID
				break
			}
		}
	}
	return(returnID)
	
}


mergeTreesByID <- function(currentID, neighborID, las_dt){

	## assign all points with currentID to have neighborID
	las_dt[ID == currentID, ID := neighborID]
	
	return(las_dt)
}

update_crown_metrics <- function(head, las_dt, cm, currentID, neighborID){
	## update cm to do the following:
		## remove currentID entry
		## re-compute the centroid, area, etc for neighborID
		newPts <- las_dt[ID == neighborID,,]
		cm_new <- crown_metrics(lidR::LAS(newPts, head), geom = "concave", func = NULL, attribute = "ID")
     		cm_new$area <- st_area(cm_new)
     		cm_new$centroid <- st_centroid(cm_new)
		currTree <- cm[ID == currentID,,]
		newTree <- cm[ID == neighborID,,]
		newMaxZ <-  max(currTree$maxZ, newTree$maxZ)
		cm_new$maxZ <- newMaxZ
    		cm_new$minDiam <- (h2cw_ratio(cm_new$maxZ)*cm_new$maxZ)
     		cm_new$minArea <- ((cm_new$minDiam/2)^2)*pi
		#print("new formatted row: ")
		#print(cm_new)
		#print("old formatted row: ")
		#print(newTree)
		#remove current ID
		cm <- cm[ID != currentID]
		cm_new <- as.data.table(cm_new)
		#add this new version of the merged tree as a row to cm and return it
		cm[ID == neighborID, names(cm_new) := cm_new]
		return(cm)

}
## manually compute 3d centroids because sf really only likes 2d centroids
compute_3d_centroids <- function(data_table){

	## two versions of dt columns: one for subsetting points by ID and another to isolate only the coordinates to pass to colMeans
	dt <- data_table
	coords_id_cols <- c('X','Y','Z','ID')
	dt <- dt[,..coords_id_cols] 
	coords_cols <- c('X','Y','Z')
	# get list of ids
	byid <- dt[, .(.N), by = ID]
	# make a list to hold results
	centroids <- list()#st_sfc()#numeric(nrow(byid))


	for(tree in 1:length(byid[,ID])){
		## get just the ID for the tree
		tree <- byid[tree]
		tree <- tree$ID
		## isolate just the points in the cloud that belong to that tree, and then drop the ID column
		pts <- dt[ID == tree,,]
		pts <- pts[, ..coords_cols]

		## build an sf object for each point and then combine into a single sf object
		sf_pts <- st_as_sf(pts, coords = coords_cols)
		sf_combined <- st_combine(sf_pts)
		## extract the coordinates and then compute column means only on the first three columns (XYZ)
		## this is necessary because st_coordinates() returns four values (XYZ M),
	        ## where M is ancillary information that SF needs but we do not
		coords <- st_coordinates(sf_combined)
		centroid <- colMeans(coords[,1:3])
		## finally, convert the result back to an sf object and add it to a list of them
		centroid <- st_point(centroid)
	#	print(paste0('Tree ID: ',tree))#,' centroid: ', centroid))
	#	print(centroid)
		centroids <- c(centroids,list(st_sfc(centroid)))
	}
	## combine all centroids into one obj
	centroids <- do.call(c, centroids)
	return(centroids)
}

mergeBelowThreshold <- function(head, las, heightThreshold){
    
     # save NAs separately to add back later
     las_nas <- filter_poi(las, ID == 99999)
     
     # remove NAs
     las <- filter_poi(las, ID != 99999)

     las_dt <- lidR::payload(las) %>% as.data.table

     #filter to only include trees below the height threshold
     
     maxHeightByID <- las_dt[, .(max(Z)), by = ID]
     treesBelowThreshold <- maxHeightByID[V1 < heightThreshold,,]
     las_subset <- filter_poi(las, ID %in% treesBelowThreshold$ID)

     #compute X-Y polygon of each tree, get its area and centroid for merging ops, and add the max-height
     cm <- crown_metrics(las_subset, attribute = "ID", geom = "concave", func = NULL)
     cm$area <- st_area(cm)
     cm$centroid <- st_centroid(cm)
     cm <- cm %>% as.data.table	
     cm[treesBelowThreshold, maxZ := V1, on = "ID"]
     cm$minDiam <- (h2cw_ratio(cm$maxZ)*cm$maxZ)
     cm$minArea <- ((cm$minDiam/2)^2)*pi
    # print(cm)
     ##
     ##  Wrap everything above here into a separate function

     ## add the min area as a column

     dynamic_trees <- lidR::payload(las) %>% as.data.table
     originalIDs <- cm$ID

     for (tree in 1:length(originalIDs)){
	tree <- originalIDs[tree]
	treeStats <- cm[ID == tree,,]
	## merge if the segmented area is less than the area of the regression equation	
	if(as.numeric(treeStats$area) < as.numeric(treeStats$minArea)){
	## merging
# 	mergeID <-getMergeID(tree, treeStats$centroid.geometry, treeStats$minDiam/2, cm)

	mergeID <-getMergeIDwithAngle(tree, treeStats$centroid.geometry, las_dt, treeStats$minDiam/2, cm)
	if(is.null(mergeID)){
		print(paste0('no merge candidate found for tree ID: ',tree))
		next
	}
	print(paste0('treeID: ',tree,' merge ID found:',mergeID))
	las_dt <- mergeTreesByID(tree, mergeID, las_dt)
	#print('finished mergeTreesByID')
	cm <- update_crown_metrics(head, las_dt, cm, tree, mergeID)
	#minD <- compute_min_centroid_dist(cm$centroid.geometry)
	#centroids <- compute_3d_centroids(las_dt)
	#print("3d centroids")
	#print(centroids)
	#minD3d <- compute_min_centroid_dist(centroids)
	#print("minimum distances for XYZ centroids")
	#print(minD3d)
	#print("minimum distances from XY centroids")
	#print(minD)
	#df <- data.frame( ID = originalIDs,
	#		 Min_XY_centroid_dist = minD,
	#		 Min_XYZ_centroid_dist =minD3d)
	#print(df)
	#write.csv(df, 'teak_043_min_centroid_dist.csv')
	##print(length(originalIDs))
	next
	}
	else{ 
	  next
	}

     }
	## join back as LAS
     	# join las_dt and las_nas, then use LAS()
     	las_nas <- as.data.table(payload(las_nas))
     	# because the header already has 99999 set as the NA value, it seems to be excluding those points.
        # here we assign NA to the column, which will be replaced with 99999 by the LAS() function.
        las_nas[, ID := NA]
        joined <- rbind(las_nas,las_dt)
	#print(las_nas)
	flas <- LAS(joined, head)
	return(flas)
	#	saveRDS(las_dt, "merge_test_043_full.rds")
}
mergeAboveThreshold <- function(head, las, heightThreshold){
    
     # save NAs separately to add back later
     las_nas <- filter_poi(las, ID == 99999)
     
     # remove NAs
     las <- filter_poi(las, ID != 99999)

     las_dt <- lidR::payload(las) %>% as.data.table

     #filter to only include trees below the height threshold
     
     maxHeightByID <- las_dt[, .(max(Z)), by = ID]
     treesBelowThreshold <- maxHeightByID[V1 > heightThreshold,,]
     las_subset <- filter_poi(las, ID %in% treesBelowThreshold$ID)

     #compute X-Y polygon of each tree, get its area and centroid for merging ops, and add the max-height
     cm <- crown_metrics(las_subset, attribute = "ID", geom = "concave", func = NULL)
     cm$area <- st_area(cm)
     cm$centroid <- st_centroid(cm)
     cm <- cm %>% as.data.table	
     cm[treesBelowThreshold, maxZ := V1, on = "ID"]
     cm$minDiam <- (h2cw_ratio(cm$maxZ)*cm$maxZ)
     cm$minArea <- ((cm$minDiam/2)^2)*pi
    # print(cm)
     ##
     ##  Wrap everything above here into a separate function

     ## add the min area as a column

     dynamic_trees <- lidR::payload(las) %>% as.data.table
     originalIDs <- cm$ID

     for (tree in 1:length(originalIDs)){
	tree <- originalIDs[tree]
	treeStats <- cm[ID == tree,,]
	## merge if the segmented area is less than the area of the regression equation	
	if(as.numeric(treeStats$area) < as.numeric(treeStats$minArea)){
	## merging
	mergeID <-getMergeID(tree, treeStats$centroid.geometry, treeStats$minDiam/2, cm)
	if(is.null(mergeID)){
		print(paste0('no merge candidate found for tree ID: ',tree))
		next
	}
	print(paste0('treeID: ',tree,' merge ID found:',mergeID))
	las_dt <- mergeTreesByID(tree, mergeID, las_dt)
	#print('finished mergeTreesByID')
	cm <- update_crown_metrics(head, las_dt, cm, tree, mergeID)
	#minD <- compute_min_centroid_dist(cm$centroid.geometry)
	#centroids <- compute_3d_centroids(las_dt)
	#print("3d centroids")
	#print(centroids)
	#minD3d <- compute_min_centroid_dist(centroids)
	#print("minimum distances for XYZ centroids")
	#print(minD3d)
	#print("minimum distances from XY centroids")
	#print(minD)
	#df <- data.frame( ID = originalIDs,
	#		 Min_XY_centroid_dist = minD,
	#		 Min_XYZ_centroid_dist =minD3d)
	#print(df)
	#write.csv(df, 'teak_043_min_centroid_dist.csv')
	##print(length(originalIDs))
	next
	}
	else{ 
	  next
	}

     }
	## join back as LAS
     	# join las_dt and las_nas, then use LAS()
     	las_nas <- as.data.table(payload(las_nas))
     	# because the header already has 99999 set as the NA value, it seems to be excluding those points.
        # here we assign NA to the column, which will be replaced with 99999 by the LAS() function.
        las_nas[, ID := NA]
        joined <- rbind(las_nas,las_dt)
	#print(las_nas)
	flas <- LAS(joined, head)
	return(flas)
	#	saveRDS(las_dt, "merge_test_043_full.rds")
}



plotIDs <- c("TEAK_043",'TEAK_044','TEAK_045','TEAK_046','TEAK_047')
for (plot in plotIDs) {
     #file <- filepath
     #fname <- extract_filename(file)
     #print(paste0("trying file: ",fname))	
     #filecount = filecount + 1
     file <- paste0("../las_outputs/norm_",plot,"_lidar_2021.las.las")

     f_head <- readLASheader(file)
     f_las <- readLAS(file)
	
     print(paste('merging file:',file))
     mergedLAS <- mergeBelowThreshold(f_head, f_las, 100)
     #mergedLAS <- mergeAboveThreshold(f_head, mergedLAS, 7)

     condition_descriptor <- 'angle_below_70'
     outfile <- paste0('../merged_las_outs/angle/',plot,condition_descriptor,'merged.las')
     writeLAS(mergedLAS, outfile)
     # but probably remember that I will need a new header if so.
     # but maybe not, cause just saving as NAs anyway

#     f_dt <- lidR::payload(f_las) %>% as.data.table
#     
#     point_clouds <- MeanShiftR::split_BufferedPointCloud(f_dt, plot.width = P_WIDTH, buffer.width = P_BUFFER)
#     print("Point cloud generation done")
#
#     lib_path <- .libPaths()[1]
#
#     tryCatch(
#	      {
#	ms_result <- MeanShiftR::parallel_MeanShift(point_clouds, lib.path = lib_path, frac.cores = 0.9, version = 'classic', H2CW = H2CW, H2CL = H2CD,minz=MINZ)
#
#     # ms_result is a data.table
#     print("Mean Shift segmentation done")
#     ms_out <- paste0("intermediates/ms_result_",strsplit(fname,'\\.')[[1]][1],".rds")
#     print(ms_out)
#     
#     
#     #10 point min
#     byid <- ms_result[, .(.N), by = ID]
#     g10 <- byid[N>10,,]
#     tg10 <- ms_result[ID %in% g10$ID]
#     ms_result <- tg10 
#     
#     saveRDS(ms_result,ms_out)
#     print("joining ms result to original data")
#     # make IDs from xyz coords
#     f_dt[, concat := paste(X,Y,Z, sep = "_")]
#     ms_result[, concat := paste(X,Y,Z, sep = "_")]
#
#     ms_result[, ID := ID]
#     # left join (update-by-reference-join) (stackoverflow)
#     # adds treeID to original lidR payload
#
#     f_dt[ms_result, on = "concat", ID := ID]
# 
#     # save meanshift-generated IDs to LAS format
#     flas <- LAS(f_dt, f_head)
#
#     flas <- add_lasattribute_manual(flas, f_dt[,ID], name = "ID", desc = "tree ID", type = "int64", NA_value = 99999)
#
#     print("saving file")
#     #outfile <- paste0("las_outputs/", fname,".las")
#     #above: full plots. Below: 400s
#     outfile <- paste0("las_outputs/400_las_outs/", fname,".las")
#     writeLAS(flas, outfile)
#     },
#     error = function(e){
#     	print(paste0("error in file: ", fname))
#	print(e)
#     	skipped[file]
#     })
#
}
## for post processing:

## take results data table, do a group by ID (in tree seg notes), save to new dt (by_id)
## do not save separately, just re-calculate in post-processing. 
## subset the groupby to only IDs with at least X points (10). save to new dt

## add maxZ to byID 
print("Skipped files: ")
print(skipped)

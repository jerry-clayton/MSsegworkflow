library(sf)
library(lidR)
library(data.table)
library(ggplot2)

output_directory <- "../resultgraphs/merged/above_below/" 
side_by_side_hist <- function(segmented, field, descriptor, siteDescriptor, nbins){

	fieldCount <- length(field)
	segCount <- length(segmented)

	m1 <- paste("Ground Truth Dist. of", descriptor,"at", siteDescriptor,'N =',fieldCount)
	m2 <- paste("Segmented Dist. of", descriptor, "at",siteDescriptor, 'N =', segCount)

	fname <- paste0(output_directory,descriptor,'_', siteDescriptor,".jpeg")
	xmax <- max(as.numeric(field), as.numeric(segmented))
	fhist <- hist(field, main = m1, breaks = nbins, xlab = descriptor, ylab = "Frequency", col = "lightgreen",xlim = c(0,xmax), cex.axis = 2, cex.main = 1.5)

	shist <- hist(segmented, main = m2, breaks = nbins, xlab = descriptor, ylab = "Frequency", xlim = c(0,xmax), col = "lightblue", cex.axis = 2, cex.main = 1.5)

	ymax <- max(fhist$counts, shist$counts)
	jpeg(fname,width = 1200, height = 800)
	par(mfrow=c(1,2))

	hist(field, main = m1, breaks = nbins, xlab = descriptor, ylab = "Frequency", col = "lightgreen",xlim = c(0,xmax), ylim = c(0,ymax), cex.axis = 2, cex.main = 1.5)

	hist(segmented, main = m2, breaks = nbins, xlab = descriptor, ylab = "Frequency", xlim = c(0,xmax), ylim = c(0,ymax), col = "lightblue", cex.axis = 2, cex.main = 1.5)
dev.off()
return(fname)
}


side_by_side_scatter <- function(segmentedH, fieldH, segmentedD, fieldD, siteDescriptor){

	fieldCount <- length(fieldH)
	segCount <- length(segmentedH)

	m1 <- paste("Ground Truth Crown Diam. vs. Crown Height at", siteDescriptor, 'N =', fieldCount)
	m2 <- paste("Segmented Crown Diam. vs. Crown Height at", siteDescriptor, "N =", segCount)

	fname <- paste0(output_directory,"scatterplot_", siteDescriptor,".jpeg")
	jpeg(fname,width = 1200, height = 800)
	par(mfrow=c(1,2))
	xmax <- max(fieldD, segmentedD)
	ymax <- max(fieldH, segmentedH)
	plot(fieldD, fieldH, main = m1, pch=19, xlab = "Crown Diameter", ylab = "Height", col = "lightgreen", xlim = c(0,xmax), ylim = c(0,ymax), cex = 1.5, cex.axis = 2, cex.main = 1.5)

	plot(segmentedD, segmentedH, main = m2, pch=19, xlab = "Crown Diameter", ylab = "Height", col = "lightblue", xlim = c(0,xmax), ylim = c(0,ymax), cex = 1.5, cex.axis = 2, cex.main = 1.5)
dev.off()
return(fname)
}
extract_filename <- function(filepath){
	
	return(strsplit(filepath, '/')[[1]][2])

}

# use all available processor cores
set_lidr_threads(0)

#filenames <- Sys.glob("../merged_rds/*.rds")



csv <- as.data.table(read.csv('../las_inputs/TEAK_entries_perplot_400.csv'))


plotIDs <-	c('TEAK_043','TEAK_044','TEAK_045','TEAK_046','TEAK_047')#unique(csv$plotID_x)

for (plot in plotIDs){

	fname <- paste0("../merged_las_outs/above_below/",plot,"above_below_7merged.las")
	print(paste("loading",fname))
#	plot <- paste0('TEAK_',plot)	
	gt <- csv[plotID_x == plot,,]
	#print(paste("Field trees before subset:", nrow(gt)))
	#gt$area <- ((gt$maxCrownDiameter/2)**2)*pi
	#gt <- gt[area > 2,,]
	las <- readLAS(fname)
	las <- filter_poi(las, ID != 99999)
	dt <- as.data.table(las@data)
	basal_height <- dt[, min(Z), by = ID]
	height <- dt[, max(Z), by = ID]
	
	cm <- crown_metrics(las, geom = "concave", func = NULL, attribute = "ID")
	cm$area <- st_area(cm)
	cm$diam <- 2*sqrt(cm$area/pi)
	area_filtered <- cm #cm[as.numeric(cm$area) > 2,,]
#	print('before height filter')

#	print(nrow(height))

	height <- height[ID %in% area_filtered$ID,,]
#	print('after height filter')
#	print(nrow(height))
	#print(paste("Field trees:", nrow(gt), "Segmented trees: ",nrow(area_filtered)))
	basal_height <- basal_height[ID %in% area_filtered$ID,,]
	cm <- area_filtered
	print('making histograms')

	#side_by_side_hist(basal_height$V1,gt$baseCrownHeight,"Basal_Height",plot,8)

	side_by_side_hist(height$V1, gt$height, "Max_Height",plot,8)
	side_by_side_hist(cm$diam, gt$maxCrownDiameter, "Crown_Diameter",plot,8)
	side_by_side_scatter(height$V1, gt$height, cm$diam, gt$maxCrownDiameter,plot)
}
# for each plot with results
# load results from LAS
# load results from CSV



## color utilities for graphing lidar point clouds 

library(RColorBrewer)

## get all available colors from the graphics device
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]


## Function to export color palette of custom length for plotting trees. 
generateCustLengthPal <- function(length){

	##randomize the color order 
	randcolor <- sample(color)
	
	# Calculate how many times the palette needs to be repeated
	num_repeats <- ceiling(length / length(randcolor))

	# Create the extended palette by repeating the original palette
	extended_palette <- rep(randcolor, num_repeats)

	# Trim the extended palette to match the exact number of items
	extended_palette <- extended_palette[1:length]
	return(extended_palette)
}

## usage example
## plot(classic_gr_300ft, color="ID", pal = extended_palette, nbreaks = 95000)


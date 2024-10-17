###
# Set parameters for MS segmentation here
#
###
# Plot width for subplots
P_WIDTH <- 200
# Plot buffer for subplots
P_BUFFER <- 20
# Height-to-crown-depth ratio for Mean Shift Algorithm
H2CD <- 0.85
# Height-to-crown-width ratio for Mean Shift Algorithm
H2CW <- 0.20

MINZ <- 1

library(lidR)
library(MeanShiftR)
library(data.table)
library(sf)

extract_filename <- function(filepath){
	
	return(strsplit(filepath, '/')[[1]][2])

}

# args <- commandArgs(trailingOnly = TRUE)
# # Check if arguments are provided
# if (length(args) == 0) {
#   stop("No arguments provided")
# }
# # Print the arguments
# print(args)
# # Example: Access individual arguments
# arg1 <- args[1]

# use all available processor cores
set_lidr_threads(0)

filenames <- Sys.glob("input/*.las")

filecount <- 0

skipped <- list()

# iterate over each file, sub-divide into point clouds
# with user-defined plot width and buffers 
# run MS on each file

## probably need to break this down into functions.

for (file in filenames) {
     #file <- filepath
     fname <- extract_filename(file)
     print(paste0("trying file: ",fname))	
     filecount = filecount + 1

     f_head <- readLASheader(file)
     f_las <- readLAS(file)

     # if we want to subset (drop ground/near ground), do it here
     # but probably remember that I will need a new header if so.
     # but maybe not, cause just saving as NAs anyway

     f_dt <- lidR::payload(f_las) %>% as.data.table
     
     point_clouds <- MeanShiftR::split_BufferedPointCloud(f_dt, plot.width = P_WIDTH, buffer.width = P_BUFFER)
     print("Point cloud generation done")

     lib_path <- .libPaths()[1]

     tryCatch(
	      {
	ms_result <- MeanShiftR::parallel_MeanShift(point_clouds, lib.path = lib_path, frac.cores = 0.9, version = 'classic', H2CW = H2CW, H2CL = H2CD,minz=MINZ)

     # ms_result is a data.table
     print("Mean Shift segmentation done")
     #ms_out <- paste0("intermediates/ms_result_",strsplit(fname,'\\.')[[1]][1],".rds")
     #print(ms_out)
     
     
     #10 point min
     byid <- ms_result[, .(.N), by = ID]
     g10 <- byid[N>10,,]
     tg10 <- ms_result[ID %in% g10$ID]
     ms_result <- tg10 
     
     #saveRDS(ms_result,ms_out)
     print("joining ms result to original data")
     # make IDs from xyz coords
     f_dt[, concat := paste(X,Y,Z, sep = "_")]
     ms_result[, concat := paste(X,Y,Z, sep = "_")]

     ms_result[, ID := ID]
     # left join (update-by-reference-join) (stackoverflow)
     # adds treeID to original lidR payload

     f_dt[ms_result, on = "concat", ID := ID]
 
     # save meanshift-generated IDs to LAS format
     flas <- LAS(f_dt, f_head)

     flas <- add_lasattribute_manual(flas, f_dt[,ID], name = "ID", desc = "tree ID", type = "int64", NA_value = 99999)

     print("saving file")
     outfile <- paste0("output/", fname,".las")
     writeLAS(flas, outfile)
     },
     error = function(e){
     	print(paste0("error in file: ", fname))
	print(e)
     	skipped[file]
     })

}
## for post processing:

## take results data table, do a group by ID (in tree seg notes), save to new dt (by_id)
## do not save separately, just re-calculate in post-processing. 
## subset the groupby to only IDs with at least X points (10). save to new dt

## add maxZ to byID 
print("Skipped files: ")
print(skipped)

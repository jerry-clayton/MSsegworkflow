install.packages('lidR', repos="https://cloud.r-project.org")
install.packages('devtools', repos="https://cloud.r-project.org")
install.packages('data.table', repos="https://cloud.r-project.org")
install.packages('sf', repos="https://cloud.r-project.org")
library(devtools)
install_version('rgeos','0.6-4', repos="https://cloud.r-project.org")
install_github('niknap/MeanShiftR')



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
H2CW <- 0.15

MINZ <- 1

library(lidR)
library(MeanShiftR)
library(data.table)
library(sf)


args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]


# use all available processor cores
set_lidr_threads(0)

# run MS on file

f_head <- readLASheader(input_file)
f_las <- readLAS(input_file)

     # if we want to subset (drop ground/near ground), do it here
     # but probably remember that I will need a new header if so.
     # but maybe not, cause just saving as NAs anyway

f_dt <- lidR::payload(f_las) %>% as.data.table
     
point_clouds <- MeanShiftR::split_BufferedPointCloud(f_dt, plot.width = P_WIDTH, buffer.width = P_BUFFER)
print("Point cloud generation done")

lib_path <- .libPaths()[1]

tryCatch({
	ms_result <- MeanShiftR::parallel_MeanShift(point_clouds, lib.path = lib_path, frac.cores = 0.9, version = 'classic', H2CW = H2CW, H2CL = H2CD,minz=MINZ)

     print("Mean Shift segmentation done")
     
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



### add merge code here






    
     print("saving file to")
     writeLAS(flas, output_file)
     },
     error = function(e){
     	print(paste0("error in file: ", fname))
	print(e)
     	skipped[file]
     })

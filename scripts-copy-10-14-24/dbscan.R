# Load necessary libraries

library(lidR)

library(geometry)  # For 3D convex hull calculation

library(rgl)       # For 3D visualization

library(dbscan)

# 1. Read the LAS file and segment trees

las <- readLAS("../merged_las_outs/TEAK_044all_Zmerged.las")



# Ensure the LAS file was properly loaded

if (is.null(las)) stop("Error: LAS file not loaded properly")

# 2. Extract individual tree points with XYZ coordinates

tree_points <- filter_poi(las, ID != 99999)# Extract only points belonging to trees


coords <- cbind(tree_points@data$X, tree_points@data$Y, tree_points@data$Z)  # Use cbind to create a matrix
coords <- data.frame(coords)

eps_value <- 0.5
minPts_value <- 15

dbscan_result <- dbscan(coords, eps = 1.0, minPts = 20)

coords$cluster <- dbscan_result$cluster+1
saveRDS(coords, 'dbscan_t44_test_eps_1_minpts_20.rds')

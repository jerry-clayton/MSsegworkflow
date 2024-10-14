# Load necessary libraries

library(lidR)

library(geometry)  # For 3D convex hull calculation

library(rgl)       # For 3D visualization



# 1. Read the LAS file and segment trees

las <- readLAS("../merged_las_outs/TEAK_044all_Zmerged.las")



# Ensure the LAS file was properly loaded

if (is.null(las)) stop("Error: LAS file not loaded properly")

# 2. Extract individual tree points with XYZ coordinates

tree_points <- filter_poi(las, ID != 99999)# Extract only points belonging to trees



# 3. Compute 3D convex hull for each tree using ID

tree_ids <- unique(tree_points$ID)  # Use 'ID' instead of 'treeID'

hull_list <- list()

for (tree_id in tree_ids) {

	  # Extract points for the current tree based on 'ID'

	  tree_data <- filter_poi(tree_points, ID == tree_id)



  # Check the number of points

  n_points <- nrow(tree_data)

    message(paste("Processing tree ID:", tree_id, "- Number of points:", n_points))

    

    # Check if there are enough points for a convex hull (at least 4 non-collinear points)

    if (n_points > 3) {  # Convex hull requires at least 4 points in 3D space

	        # Extract XYZ coordinates using cbind

	        coords <- cbind(tree_data@data$X, tree_data@data$Y, tree_data@data$Z)  # Use cbind to create a matrix



        # Check variance to assess collinearity

        var_x <- var(coords[, 1])

	    var_y <- var(coords[, 2])

	    var_z <- var(coords[, 3])

	        message(paste("Variance for tree ID:", tree_id, "- X:", var_x, "Y:", var_y, "Z:", var_z))

	        

	        # Print coordinates for debugging

	        message(paste("Coordinates for tree ID:", tree_id))

		    print(head(coords))  # Show the first few points



		    # Compute the convex hull

		    hull <- try(convhulln(coords, options = "FA"), silent = TRUE)

		        if (inherits(hull, "try-error")) {

				      message(paste("Error computing convex hull for ID:", tree_id, "-", attr(hull, "condition")$message))

		          hull <- NULL  # Reset hull to NULL if there was an error

			      }



		        # Check if hull is valid and has rows

		        if (!is.null(hull) && is.matrix(hull) && nrow(hull) > 0) {

				      hull_list[[as.character(tree_id)]] <- list(coords = coords, hull = hull)

			    } else {

				          message(paste("No valid hull returned for ID:", tree_id))

			        }

			  } else {

				      message(paste("Not enough points for convex hull for ID:", tree_id))

			    }

}



# 4. Visualize the 3D convex hulls and points

if (length(hull_list) > 0) {

	  open3d()

  

  # Plot all tree points

  points3d(tree_points@data$X, tree_points@data$Y, tree_points@data$Z, col = "darkgreen", size = 1)  # Plot all points

    

    for (hull_data in hull_list) {

	        # Extract the points and hull for visualization

	        coords <- hull_data$coords

      hull <- hull_data$hull

          

          # Plot the convex hull triangles

          if (!is.null(hull) && is.matrix(hull)) {

		        for (i in seq_len(nrow(hull))) {

				        triangles3d(coords[hull[i, ], ], col = "blue", alpha = 0.3)  # Draw the hull faces

            }

          } else {

		        message("Invalid hull data; skipping visualization.")

	      }

        }

    

    # Keep the window open

    rglwidget()  # This will render the plot in a web browser

} else {

	  message("No valid convex hulls were computed.")

}

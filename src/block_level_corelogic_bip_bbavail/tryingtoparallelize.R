

library(parallel)

st_par <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  # Combine results back together. Method of combining depends on the output from the function.
  if ( class(split_results[[1]]) == 'list' ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else {
    result <- do.call("rbind", split_results)
  }
  
  # Return result
  return(result)
}

detectCores()

state_centroids <- st_centroid(state_blocks)
centroid_par <- st_par(state_blocks, st_centroid, detectCores() - 1) 

state_centroids
centroid_par <- centroid_par %>% st_as_sf()

identical(state_centroids$geometry, centroid_par$geometry)
identical(head(state_centroids$geometry), head(centroid_par$geometry))

length(setdiff(state_centroids$geometry, centroid_par$geometry))
length(setdiff(centroid_par$geometry, state_centroids$geometry))

length(setdiff(state_centroids, centroid_par))
length(setdiff(centroid_par$geometry, state_centroids$geometry))

plot(state_centroids$geometry, col = "blue", pch = 19)
plot(centroid_par$geometry, col = "red", pch = 15)


n <- detectCores() - 2
alldistances <- st_distance(nearest_bips, state_centroids) #rows = centroids #cols = BIPS
distance_par <- st_par(sf_df = head(nearest_bips),sf_func =  st_distance, n_cores = n, y = head(state_centroids)) 

identical(alldistances, distance_par)

head(alldistances)
head(distance_par)


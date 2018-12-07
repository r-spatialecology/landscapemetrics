landscape_matrix <- raster::as.matrix(landscape)

landscape_stack <- raster::stack(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_brick <- raster::brick(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_list <- list(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_simple <- landscapemetrics::landscape
landscape_simple[raster::values(landscape_simple) == 2] <- 1

landscape_uniform <- landscapemetrics::landscape
raster::values(landscape_uniform) <- 1

landscape_diff_res <- raster::aggregate(landscapemetrics::landscape, fact = c(1,2))

# landscape_stars <- stars::st_as_stars(landscape)

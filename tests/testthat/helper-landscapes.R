landscape_stack <- raster::stack(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_brick <- raster::brick(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_list <- list(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_simple <- landscape
landscape_simple[raster::values(landscape_simple) == 2] <- 1

# landscape_stars <- stars::st_as_stars(landscape)

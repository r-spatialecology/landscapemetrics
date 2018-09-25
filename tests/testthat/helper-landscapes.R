landscape_stack <- raster::stack(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_brick <- raster::brick(landscapemetrics::landscape, landscapemetrics::landscape)

landscape_list <- list(landscapemetrics::landscape, landscapemetrics::landscape)

# landscape_stars <- stars::st_as_stars(landscape)

landscape_stack <- raster::stack(landscape, landscape)

landscape_brick <- raster::brick(landscape, landscape)

landscape_list <- list(landscape, landscape)

landscape_simple <- landscape
landscape_simple[raster::values(landscape_simple) == 2] <- 1

landscape_uniform <- landscape
raster::values(landscape_uniform) <- 1

# landscape_stars <- stars::st_as_stars(landscape)

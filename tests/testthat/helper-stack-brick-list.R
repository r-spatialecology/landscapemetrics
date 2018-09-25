landscape_stack <- raster::stack(landscape, landscape)

landscape_brick <- raster::brick(landscape, landscape)

landscape_list <- landscape_list

# landscape_stars <- stars::st_as_stars(landscape)

# create landscapes

landscape <- terra::rast(landscapemetrics::landscape)
augusta_nlcd <- terra::rast(landscapemetrics::augusta_nlcd)
podlasie_ccilc <- terra::rast(landscapemetrics::podlasie_ccilc)

# landscape_ras <- raster::raster(landscape)

landscape_matrix <- terra::as.matrix(landscape, wide = TRUE)

landscape_stack <- c(landscape, landscape)

landscape_list <- list(landscape, landscape)

landscape_simple <- landscape
landscape_simple[terra::values(landscape_simple, mat = FALSE) == 2] <- 1

landscape_uniform <- landscape
terra::values(landscape_uniform) <- 1

landscape_diff_res <- terra::aggregate(landscape, fact = c(1,2))

landscape_NA <- landscape
terra::values(landscape_NA) <- NA

# augusta_nlcd_stars <- stars::st_as_stars(augusta_nlcd)

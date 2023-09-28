
#### Create landscapes ####

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


#### Create points ####
sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

# points_multi <- sf::st_multipoint(sample_points)
#
# points_sf <- sf::st_sfc(points_multi, points_multi)
#
# points_sfc <- sf::st_sf(geometry = sf::st_sfc(points_multi),
#                         ID = 3)

points_terra <- terra::vect(sample_points)

# wrong plots
sample_points_wrong <- cbind(sample_points, 1)

# # use polygons
# poly_1 <- sf::st_polygon(list(cbind(c(2.5, 2.5, 17.5, 17.5, 2.5),
#                                     c(-2.5, 12.5, 12.5, -2.5, -2.5))), "p1")
#
# poly_2 <-  sf::st_polygon(list(cbind(c(7.5, 7.5, 23.5, 23.5, 7.5),
#                                      c(-7.5, 23.5, 23.5, -7.5, -7.5))), "p2")
#
# sample_plots <- sf::st_sfc(poly_1, poly_2)

# sample_plots_sp <- as(sample_plots, "Spatial")

#### import and reshape FRAGSTATS v2.0 results ####

tolerance <- 5.0

fragstats_patch <- landscapemetrics:::internal_data$fs_data$patch |>
    tidyr::pivot_longer(cols = -c("LID", "PID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_class <- landscapemetrics:::internal_data$fs_data$class |>
    tidyr::pivot_longer(cols = -c("LID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_landscape <- landscapemetrics:::internal_data$fs_data$landscape |>
    tidyr::pivot_longer(cols = -c("LID"), names_to = "metric") |>
    dplyr::mutate(metric = stringr::str_to_lower(metric))

test_diff <- function(obs, exp, tol) {

    # remove all NA values which could be present
    obs <- obs[!is.na(obs)]
    exp <- exp[!is.na(exp)]

    # check if vectors have the same length
    if (length(obs) != length(exp)) stop()

    # get id of closest value for each obs in exp
    id <- sapply(obs, function(x) which.min(abs(exp - x)))
    # MH: What if two values are equally min?

    # calculate relative differencce between now ordered vectors
    d <- ((obs - exp[id]) / exp[id]) * 100

    # value will be NaN if divided by zero
    d[obs == 0 & exp[id] == 0] <- 0

    # check if all values are below tolerance
    flag <- all(abs(d) <= tol)

    # throw warning if not
    if(!flag) warning(paste0("Largest diff=", max(round(d, 2)), "%"))

    return(flag)

}

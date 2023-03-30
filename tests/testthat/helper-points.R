sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

# points_multi <- sf::st_multipoint(sample_points)
#
# points_sp <- sp::SpatialPoints(sample_points)
#
# points_spdf <- sp::SpatialPointsDataFrame(sample_points, data = data.frame(ID = rep(1,3)))
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

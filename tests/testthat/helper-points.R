sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

points_geom <- sf::st_multipoint(sample_points)

# points_sp <- sp::SpatialPoints(matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE))
#
# points_spdf <- sp::SpatialPointsDataFrame(matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE),
#                                           data = data.frame(ID = rep(1,3)))

points_sfc <- sf::st_sfc(points_geom, points_geom)

points_sf <- sf::st_sf(geometry = sf::st_sfc(points_geom),
                       ID = 3)

# wrong plots
sample_points_wrong <- cbind(sample_points, 1)

# # use polygons
# poly_1 <-  sp::Polygon(cbind(c(2.5, 2.5, 17.5, 17.5),
#                            c(-2.5, 12.5, 12.5, -2.5)))
# poly_2 <-  sp::Polygon(cbind(c(7.5, 7.5, 23.5, 23.5),
#                            c(-7.5, 23.5, 23.5, -7.5)))
# poly_1 <- sp::Polygons(list(poly_1), "p1")
# poly_2 <- sp::Polygons(list(poly_2), "p2")

# sample_plots <- sp::SpatialPolygons(list(poly_1, poly_2))

# # use lines
# x1 <- c(1, 5, 15, 10)
# y1 <- c(1, 5, 15, 25)
#
# x2 <- c(10, 25)
# y2 <- c(5, 5)
#
# sample_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(x1, y1)),
#                                                      sp::Line(cbind(x2, y2))), ID = "a")))

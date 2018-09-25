
points_matrix <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

points_sp <- sp::SpatialPoints(matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE))

points_spdf <- sp::SpatialPointsDataFrame(matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE),
                                          data = data.frame(ID = rep(1,3)))

points_point <- sf::st_point(matrix(c(10, 5), ncol = 2, byrow = TRUE))

points_multipoint <- sf::st_multipoint(matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE))

points_sfc <- sf::st_sfc(sf::st_multipoint(matrix(c(10, 5, 25, 15), ncol = 2, byrow = TRUE)),
                     sf::st_multipoint(matrix(c(5, 25, 15, 25), ncol = 2, byrow = TRUE)))

points_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipoint(matrix(c(10, 5, 25, 15, 5, 25),
                                                                  ncol = 2, byrow = TRUE))),
                       ID = 3)

context("construct_buffer")

sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
points_sf <- sf::st_multipoint(sample_points)

test_that("construct_buffer is typestable", {

    expect_is(construct_buffer(sample_points, shape = "circle", size = 5), "sf")
    expect_is(construct_buffer(points_sf, shape = "square", size = 5), "sf")
})

test_that("construct_buffer can return a matrix", {

    expect_is(construct_buffer(sample_points,
                               shape = "circle", size = 5,
                               return_sf = FALSE), "matrix")

    expect_is(construct_buffer(points_sf,
                               shape = "circle", size = 5,
                               return_sf = FALSE), "matrix")
})

test_that("construct_buffer returns error for unkown shape", {

    shape <- "3D"

    expect_error(construct_buffer(sample_points, shape = "3D", size = 5),
                 regexp = paste0("Shape option " , shape, " unkown."),
                 fixed = TRUE)
})

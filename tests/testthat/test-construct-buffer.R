context("construct_buffer")

sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
points_sp <- sp::SpatialPoints(sample_points)

test_that("construct_buffer is typestable", {

    expect_is(construct_buffer(sample_points, shape = "circle", size = 5), "SpatialPolygons")
    expect_is(construct_buffer(points_sp, shape = "square", size = 5), "SpatialPolygons")
})

test_that("construct_buffer can return a matrix", {

    expect_is(construct_buffer(sample_points,
                               shape = "circle", size = 5,
                               return_sp = FALSE), "matrix")

    expect_is(construct_buffer(points_sp,
                               shape = "circle", size = 5,
                               return_sp = FALSE), "matrix")

})

test_that("construct_buffer returns error for unkown shape", {

    shape <- "3D"

    expect_error(construct_buffer(points_matrix, shape = "3D", size = 5),
                 regexp = paste0("Shape option " , shape, " unkown."),
                 fixed = TRUE)
})

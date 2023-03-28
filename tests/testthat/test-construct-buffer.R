context("construct_buffer")

test_that("construct_buffer is typestable", {

    expect_is(construct_buffer(sample_points, shape = "circle", size = 5), "SpatVector")

})

test_that("construct_buffer can return a matrix", {

    expect_is(construct_buffer(sample_points, shape = "circle", size = 5,
                               return_vec = FALSE), "matrix")

    expect_is(construct_buffer(points_sf, shape = "circle", size = 5,
                               return_vec = FALSE), "matrix")
})

test_that("construct_buffer returns error for unkown shape", {

    shape <- "3D"

    expect_error(construct_buffer(sample_points, shape = "3D", size = 5),
                 regexp = paste0("Shape option " , shape, " unkown."),
                 fixed = TRUE)
})

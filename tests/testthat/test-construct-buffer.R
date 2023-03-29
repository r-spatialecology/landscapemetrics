test_that("construct_buffer is typestable", {

    expect_s4_class(construct_buffer(sample_points, shape = "circle", size = 5),
                    "SpatVector")

})

test_that("construct_buffer can return a matrix", {

    circle_a <- construct_buffer(sample_points, shape = "circle", size = 5,
                                 return_vec = FALSE)

    square <- construct_buffer(points_sf, shape = "square", size = 5,
                               return_vec = FALSE)

    expect_true(is.matrix(circle_a))

    expect_true(is.matrix(square))

})

test_that("construct_buffer returns error for unkown shape", {

    shape <- "3D"

    expect_error(construct_buffer(sample_points, shape = "3D", size = 5),
                 regexp = paste0("Shape option " , shape, " unkown."),
                 fixed = TRUE)
})

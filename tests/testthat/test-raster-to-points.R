context("raster_to_points")

test_that("raster_to_points returns a row for each cell", {

    expect_equal(nrow(raster_to_points(landscape)),
                 expected = raster::ncell(landscape))
})


test_that("raster_to_points considers NAs", {

    landscape[1,1] <- NA

    expect_true(anyNA(raster_to_points(landscape)))
    expect_false(anyNA(raster_to_points(landscape, return_NA = FALSE)))
})

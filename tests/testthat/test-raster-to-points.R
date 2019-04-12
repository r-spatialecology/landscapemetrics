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


test_that("raster_to_points works for all data type", {

    result_stack <- raster_to_points(landscape_stack)
    result_brick <- raster_to_points(landscape_brick)
    result_list <- raster_to_points(landscape_list)

    expect_equal(object = nrow(result_stack),
                 expected = raster::ncell(landscape_stack) *
                     raster::nlayers(landscape_stack))

    expect_equal(object = nrow(result_brick),
                 expected = raster::ncell(landscape_brick) *
                     raster::nlayers(landscape_brick))

    expect_equal(object = nrow(result_list),
                 expected = (raster::ncell(landscape_list[[1]]) +
                                 raster::ncell(landscape_list[[2]])) / 2 *
                     length(landscape_list))
})

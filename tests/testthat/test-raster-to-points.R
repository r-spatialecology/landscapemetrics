context("raster_to_points")

test_that("raster_to_points works for all data type", {

    result_layer <- raster_to_points(landscape)
    result_stack <- raster_to_points(landscape_stack)
    result_brick <- raster_to_points(landscape_brick)
    result_list <- raster_to_points(landscape_list)

    expect_is(result_layer, class = "matrix")
    expect_is(result_stack, class = "matrix")
    expect_is(result_brick, class = "matrix")
    expect_is(result_list, class = "matrix")

    expect_equal(object = nrow(result_layer),
                 expected = raster::ncell(landscape))

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

test_that("raster_to_points considers NAs", {

    landscape_NA <- landscape
    landscape_NA[1, 1] <- NA

    result <- raster_to_points(landscape_NA, return_NA = FALSE)
    result_NA <- raster_to_points(landscape_NA, return_NA = TRUE)

    expect_true(any(!is.na(result)))
    expect_true(any(is.na(result_NA)))

    expect_equal(object = nrow(result),
                 expected = raster::ncell(landscape) - 1)
    expect_equal(object = nrow(result_NA),
                 expected = raster::ncell(landscape))
})

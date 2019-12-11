context("ununpad_raster")

test_that("unpad_raster can handle all raster inputs", {

    raster_layer <- landscapemetrics::pad_raster(landscape,
                                                 return_raster = TRUE)[[1]]

    raster_stack <- raster::stack(landscapemetrics::pad_raster(landscape_stack,
                                                               return_raster = TRUE))

    raster_brick <- raster::brick(landscapemetrics::pad_raster(landscape_brick,
                                                               return_raster = TRUE))

    raster_list <- landscapemetrics::pad_raster(landscape_list,
                                                return_raster = TRUE)

    raster_matrix <- landscapemetrics::pad_raster(landscape_matrix,
                                                  return_raster = FALSE)[[1]]

    expect_is(unpad_raster(raster_layer), "list")
    expect_is(unpad_raster(raster_stack), "list")
    expect_is(unpad_raster(raster_brick), "list")
    expect_is(unpad_raster(raster_list), "list")
    expect_is(unpad_raster(raster_matrix, return_raster = FALSE), "list")
})

test_that("unpad_raster removes correct number of rows/cols", {

    result_a <- landscapemetrics:::unpad_raster(landscape,
                                                unpad_raster_cells = 1)[[1]]


    result_b <- landscapemetrics:::unpad_raster(landscape,
                                                unpad_raster_cells = 5)[[1]]

    # original landscape is 30 x 30 and removing 5 rows/cols in each direction
    expect_equal(object = raster::nrow(result_a), expected = 28)
    expect_equal(object = raster::nrow(result_a), expected = 28)

    # original landscape is 30 x 30 and removing 5 rows/cols in each direction
    expect_equal(object = raster::nrow(result_b), expected = 20)
    expect_equal(object = raster::nrow(result_b), expected = 20)
})

test_that("unpad_raster can return RasterLayer and matrix", {

    ras <- landscapemetrics:::unpad_raster(landscape)[[1]]

    mat <- landscapemetrics:::unpad_raster(landscape, return_raster = FALSE)[[1]]

    expect_is(object = ras, class = "RasterLayer")

    expect_is(object = mat, class = "matrix")
})

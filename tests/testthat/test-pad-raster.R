context("pad_raster")

test_that("pad_raster can handle all raster inputs", {
    expect_is(landscapemetrics:::pad_raster(landscape), "list")
    expect_is(landscapemetrics:::pad_raster(landscape_stack), "list")
    expect_is(landscapemetrics:::pad_raster(landscape_brick), "list")
    expect_is(landscapemetrics:::pad_raster(landscape_list), "list")
    expect_is(landscapemetrics:::pad_raster(raster::as.matrix(landscape)), "list")
})

test_that("pad_raster can add more than one row", {

    result <- landscapemetrics:::pad_raster(landscape,
                                            pad_raster_cells = 5,
                                            global = TRUE)[[1]]

    # original landscape is 30 x 30 and adding 5 rows/cols in each direction
    expect_equal(object = raster::nrow(result), expected = 40)
    expect_equal(object = raster::nrow(result), expected = 40)
})

test_that("pad_raster can add different padding values", {

    result <- landscapemetrics:::pad_raster(landscape,
                                            pad_raster_value = NA)[[1]]

    expect_true(anyNA(raster::values(result)))
})

test_that("pad_raster can return RasterLayer and matrix", {

    raster_layer <- landscapemetrics:::pad_raster(landscape)[[1]]

    mat <- landscapemetrics:::pad_raster(landscape, return_raster = FALSE)[[1]]

    expect_is(object = raster_layer, class = "RasterLayer")

    expect_is(object = mat, class = "matrix")
})

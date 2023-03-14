context("unpad_raster")

lsm_padded <- pad_raster(landscape)[[1]]
stack_padded <- c(pad_raster(landscape_stack))
matrix_padded <- pad_raster(landscape_matrix, return_raster = FALSE)[[1]]
list_padded <- list(lsm_padded, matrix_padded)

lsm_padded_large <- pad_raster(landscape, pad_raster_cells = 5)[[1]]

test_that("unpad_raster can handle all raster inputs", {

    expect_is(object = unpad_raster(lsm_padded), class = "list")
    expect_is(object = unpad_raster(stack_padded), class = "list")
    expect_is(object = unpad_raster(list_padded), class = "list")
    expect_is(object = unpad_raster(matrix_padded, return_raster = FALSE), class = "list")
})

test_that("unpad_raster removes correct number of rows/cols", {

    result_a <- unpad_raster(lsm_padded, unpad_raster_cells = 1)[[1]]

    result_b <- unpad_raster(lsm_padded_large, unpad_raster_cells = 5)[[1]]

    # original landscape is 30 x 30 and removing 5 rows/cols in each direction
    expect_equal(object = terra::nrow(result_a), expected = 30)
    expect_equal(object = terra::nrow(result_a), expected = 30)

    # original landscape is 30 x 30 and removing 5 rows/cols in each direction
    expect_equal(object = terra::nrow(result_b), expected = 30)
    expect_equal(object = terra::nrow(result_b), expected = 30)
})

test_that("unpad_raster can return RasterLayer and matrix", {

    expect_is(object = unpad_raster(lsm_padded)[[1]],
              class = "SpatRaster")

    expect_is(object = unpad_raster(matrix_padded, return_raster = FALSE)[[1]],
              class = "matrix")
})

test_that("unpad_raster can return write to disk", {

    result <- unpad_raster(lsm_padded, to_disk = TRUE)

    expect_false(object = terra::inMemory(result[[1]]))
})

test_that("unpad_raster returns warning for matrix and return_raster = TRUE", {

    expect_warning(object = unpad_raster(matrix_padded, return_raster = TRUE),
                   regexp = "'return_raster = TRUE' or 'to_disk = TRUE' not able for matrix input.")

    expect_warning(object = unpad_raster(matrix_padded, to_disk = TRUE),
                   regexp = "'return_raster = TRUE' or 'to_disk = TRUE' not able for matrix input.")
})


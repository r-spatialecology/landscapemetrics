context("matrix_to_raster")

landscape_matrix <-terra::as.matrix(landscape, wide = TRUE)

test_that("matrix_to_raster works with provided landscape", {

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape)

    expect_is(landscape_new, class = "SpatRaster")
    expect_true(terra::ext(landscape_new) == terra::ext(landscape))
})

test_that("matrix_to_raster works with x, resolution, crs arguments", {

    landscape_new <- matrix_to_raster(matrix = landscape_matrix,
                                      extent = terra::ext(landscape),
                                      resolution = terra::res(landscape),
                                      crs = terra::crs(landscape))

    expect_is(landscape_new, class = "SpatRaster")
    expect_true(terra::ext(landscape_new) == terra::ext(landscape))
})

test_that("matrix_to_raster works with provided empty landscape", {

    landscape_empty <- terra::rast(x = terra::ext(landscape),
                                      resolution = terra::res(landscape),
                                      crs = terra::crs(landscape))

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape_empty,
                                      landscape_empty = TRUE)

    expect_is(landscape_new, class = "SpatRaster")
    expect_true(terra::ext(landscape_new) == terra::ext(landscape))
})

test_that("matrix_to_raster writes on disk", {

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape,
                                      to_disk = TRUE)

    expect_is(landscape_new, class = "SpatRaster")
    expect_true(terra::ext(landscape_new) == terra::ext(landscape))
    expect_false(terra::inMemory(landscape_new))
})

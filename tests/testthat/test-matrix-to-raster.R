context("matrix_to_raster")

landscape_matrix <- raster::as.matrix(landscape)

test_that("matrix_to_raster works with provided landscape", {

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape)

    expect_is(landscape_new, class = "RasterLayer")
    expect_true(raster::extent(landscape_new) == raster::extent(landscape))
})

test_that("matrix_to_raster works with x, resolution, crs arguments", {

    landscape_new <- matrix_to_raster(matrix = landscape_matrix,
                                      extent = raster::extent(landscape),
                                      resolution = raster::res(landscape),
                                      crs = raster::crs(landscape))

    expect_is(landscape_new, class = "RasterLayer")
    expect_true(raster::extent(landscape_new) == raster::extent(landscape))
})

test_that("matrix_to_raster works with provided empty landscape", {

    landscape_empty <- raster::raster(x = raster::extent(landscape),
                                      resolution = raster::res(landscape),
                                      crs = raster::crs(landscape))

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape_empty,
                                      landscape_empty = TRUE)

    expect_is(landscape_new, class = "RasterLayer")
    expect_true(raster::extent(landscape_new) == raster::extent(landscape))
})

test_that("matrix_to_raster writes on disk", {

    landscape_new <- matrix_to_raster(matrix =landscape_matrix,
                                      landscape = landscape,
                                      to_disk = TRUE)

    expect_is(landscape_new, class = "RasterLayer")
    expect_true(raster::extent(landscape_new) == raster::extent(landscape))
    expect_false(raster::inMemory(landscape_new))
})

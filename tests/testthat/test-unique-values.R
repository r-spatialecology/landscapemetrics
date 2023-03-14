context("get_unique_values")

test_that("get_unique_values returns values for RasterLayer", {
   expect_length(get_unique_values(landscape)[[1]], n = 3)
})

test_that("get_unique_values returns values for a matrix", {
    expect_length(get_unique_values(terra::as.matrix(landscape, wide = TRUE))[[1]], n = 3)
})

test_that("get_unique_values returns values for a matrix", {
    expect_length(get_unique_values(terra::as.matrix(landscape, wide = TRUE))[[1]], n = 3)
})

context("get_boundaries")

test_that("get_boundaries works for RasterLayer", {

    result <- get_boundaries(landscape)

    expect_is(result, "RasterLayer")
    expect_true(raster::extent(landscape) == raster::extent(result))
    expect_true(all(get_unique_values(result)[[1]] == c(0, 1)))
})

test_that("get_boundaries works for matrix", {

    result <- get_boundaries(landscape = raster::as.matrix(landscape),
                             return_raster = FALSE)

    expect_is(result, "matrix")
    expect_equal(prod(dim(result)),
                 expected = raster::ncell(landscape))
    expect_equal(get_unique_values(result)[[1]],
                 expected = c(0, 1))
})

test_that("get_boundaries returns only 1 and NA", {

    result <- get_boundaries(landscape,
                             as_NA = TRUE)
    expect_equal(get_unique_values(result)[[1]],
                 expected = 1)
})

test_that("get_boundaries works for 8 directions", {
    class_1 <- get_patches(landscape, class = 1)[[1]]
    result4 <- get_boundaries(class_1, directions = 4)
    result8 <- get_boundaries(class_1, directions = 8)

    expect_true(min(raster::getValues(result8) - raster::getValues(result4), na.rm = TRUE) >= 0)
})

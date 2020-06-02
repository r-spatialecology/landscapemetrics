context("get_boundaries")

test_that("get_boundaries works for RasterLayer", {

    result <- get_boundaries(landscape)[[1]]

    expect_is(result, "RasterLayer")
    expect_true(raster::extent(landscape) == raster::extent(result))
    expect_true(all(get_unique_values(result)[[1]] == c(0, 1)))
})

test_that("get_boundaries works for matrix", {

    result <- get_boundaries(landscape = landscape,
                             return_raster = FALSE)[[1]]

    expect_is(result, "matrix")
    expect_equal(prod(dim(result)),
                 expected = raster::ncell(landscape))
    expect_equal(get_unique_values(result)[[1]],
                 expected = c(0, 1))
})

test_that("get_boundaries works for all other data types", {

    result_stack <- get_boundaries(landscape_stack)
    result_brick <- get_boundaries(landscape_brick)
    result_list <- get_boundaries(landscape_list)

    expect_is(result_stack, "list")
    expect_is(result_brick, "list")
    expect_is(result_list, "list")
})

test_that("get_boundaries returns only 1 and NA", {

    result <- get_boundaries(landscape,
                             as_NA = TRUE)[[1]]

    expect_equal(get_unique_values(result)[[1]],
                 expected = 1)
})

test_that("get_boundaries works for 8 directions", {

    class_1 <- get_patches(landscape, class = 1)[[1]]
    result4 <- get_boundaries(class_1, directions = 4)[[1]]
    result8 <- get_boundaries(class_1, directions = 8)[[1]]

    expect_true(min(raster::getValues(result8) - raster::getValues(result4), na.rm = TRUE) >= 0)
})

test_that("get_boundaries can increase edge_depth", {

    class_1 <- get_patches(landscape, class = 1)[[1]]
    result_depth_1 <- get_boundaries(class_1, edge_depth = 1)[[1]]
    result_depth_3 <- get_boundaries(class_1, edge_depth = 3)[[1]]

    check <- sum(raster::values(result_depth_1), na.rm = TRUE) <
        sum(raster::values(result_depth_3), na.rm = TRUE)

    expect_true(object = check)
})

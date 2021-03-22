context("get_boundaries")

classes_lsm <- get_patches(landscape)[[1]]

test_that("get_boundaries works for all data types", {

    raster_layer <- get_boundaries(classes_lsm[[1]])
    raster_stack <- get_boundaries(raster::stack(classes_lsm))
    raster_brick <- get_boundaries(raster::brick(classes_lsm))
    raster_list <- get_boundaries(classes_lsm)

    expect_true(all(sapply(raster_layer, inherits, what = "RasterLayer")))
    expect_true(all(sapply(raster_stack, inherits, what = "RasterLayer")))
    expect_true(all(sapply(raster_brick, inherits, what = "RasterLayer")))
    expect_true(all(sapply(raster_list, inherits, what = "RasterLayer")))

    expect_true(raster::extent(raster_layer[[1]]) == raster::extent(landscape))

    expect_length(object = raster_list, n = length(classes_lsm))
})

test_that("get_boundaries returns matrix", {

    raster_layer <- get_boundaries(classes_lsm[[1]],
                                   return_raster = FALSE)
    raster_stack <- get_boundaries(raster::stack(classes_lsm),
                                   return_raster = FALSE)
    raster_brick <- get_boundaries(raster::brick(classes_lsm),
                                   return_raster = FALSE)
    raster_list <- get_boundaries(classes_lsm,
                                  return_raster = FALSE)

    expect_true(all(sapply(raster_layer, inherits, what = "matrix")))
    expect_true(all(sapply(raster_stack, inherits, what = "matrix")))
    expect_true(all(sapply(raster_brick, inherits, what = "matrix")))
    expect_true(all(sapply(raster_list, inherits, what = "matrix")))

    expect_length(object = raster_list, n = length(classes_lsm))
})

test_that("get_boundaries return either 1/0 or 1/NA", {

    result_10 <- get_boundaries(classes_lsm[[1]],
                                as_NA = FALSE)

    result_NA <- get_boundaries(classes_lsm[[1]],
                             as_NA = TRUE)

    expect_equal(object = get_unique_values(result_10[[1]], simplify = TRUE),
                 expected = c(0, 1))

    expect_equal(object = get_unique_values(result_NA[[1]], simplify = TRUE),
                 expected = 1)
})

test_that("get_boundaries can increase edge_depth", {

    result_depth_1 <- get_boundaries(classes_lsm[[1]], edge_depth = 1)
    result_depth_3 <- get_boundaries(classes_lsm[[1]], edge_depth = 3)

    check <- sum(raster::values(result_depth_1[[1]]), na.rm = TRUE) <
        sum(raster::values(result_depth_3[[1]]), na.rm = TRUE)

    expect_true(object = check)
})

test_that("get_boundaries can use original patch id", {

    result <- get_boundaries(classes_lsm[[1]], patch_id = TRUE)

    expect_equal(object = get_unique_values(result[[1]], simplify = TRUE),
                 expected = c(0, get_unique_values(classes_lsm[[1]],
                                                   simplify = TRUE)))
})

test_that("get_boundaries can consider boundary", {

    result <- get_boundaries(classes_lsm[[1]], consider_boundary = FALSE)
    result_boundary <- get_boundaries(classes_lsm[[1]], consider_boundary = TRUE)

    check <- sum(raster::values(result[[1]]), na.rm = TRUE) >
        sum(raster::values(result_boundary[[1]]), na.rm = TRUE)

    expect_true(object = check)

})

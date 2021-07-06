context("get_centroids")

test_that("get_centroids runs for all data types", {

    raster_layer <- get_centroids(landscape)
    raster_stack <- get_centroids(landscape_stack)
    raster_brick <- get_centroids(landscape_brick)
    raster_list <- get_centroids(landscape_list)

    expect_is(raster_layer, "tbl_df")
    expect_is(raster_stack, "tbl_df")
    expect_is(raster_brick, "tbl_df")
    expect_is(raster_list, "tbl_df")
})

test_that("get_centroids returns in every column the correct type", {

    centroids <- get_centroids(landscape)

    expect_type(centroids$layer, "integer")
    expect_type(centroids$level, "character")
    expect_type(centroids$class, "integer")
    expect_type(centroids$id, "integer")
    expect_type(centroids$x, "double")
    expect_type(centroids$y, "double")
})

test_that("get_centroids returns centroid for each patch", {

    centroids <- get_centroids(landscape)

    np <- lsm_l_np(landscape)

    expect_true(object = nrow(centroids) == np$value)
})

test_that("get_centroids allows to set cell_center", {

    expect_warning(get_centroids(landscape, cell_center = TRUE),
                   regexp = "For some patches several cell centers are returned as centroid.")

    centroids <- get_centroids(landscape, cell_center = TRUE,
                                  verbose = FALSE)

    np <- lsm_l_np(landscape)

    expect_true(object = nrow(centroids) > np$value)
})

test_that("get_centroids can return sp", {

    centroids_sp <- get_centroids(landscape, cell_center = TRUE,
                                  return_sp = TRUE)

    expect_is(centroids_sp, "SpatialPointsDataFrame")
})

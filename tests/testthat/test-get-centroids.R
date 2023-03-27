context("get_centroids")

centroids <- get_centroids(landscape, verbose = FALSE)

test_that("get_centroids runs for all data types", {

    raster_layer <- get_centroids(landscape, verbose = FALSE)
    raster_stack <- get_centroids(landscape_stack, verbose = FALSE)
    raster_list <- get_centroids(landscape_list, verbose = FALSE)

    expect_is(raster_layer, "tbl_df")
    expect_is(raster_stack, "tbl_df")
    expect_is(raster_list, "tbl_df")
})

test_that("get_centroids returns in every column the correct type", {

    expect_type(centroids$layer, "integer")
    expect_type(centroids$level, "character")
    expect_type(centroids$class, "integer")
    expect_type(centroids$id, "integer")
    expect_type(centroids$x, "double")
    expect_type(centroids$y, "double")
})

test_that("get_centroids returns centroid for each patch", {

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

test_that("get_centroids can return sf", {

    centroids_spat <- get_centroids(landscape, cell_center = TRUE, return_vec = TRUE,
                                  verbose = FALSE)

    expect_is(centroids_spat, "SpatVector")
})

context("get_centroid")

test_that("get_centroid runs for all data types", {

    raster_layer <- get_centroid(landscape)
    raster_stack <- get_centroid(landscape_stack)
    raster_brick <- get_centroid(landscape_brick)
    raster_list <- get_centroid(landscape_list)

    expect_is(raster_layer, "tbl_df")
    expect_is(raster_stack, "tbl_df")
    expect_is(raster_brick, "tbl_df")
    expect_is(raster_list, "tbl_df")
})

test_that("get_centroid returns in every column the correct type", {

    raster_layer <- get_centroid(landscape)

    expect_type(raster_layer$layer, "integer")
    expect_type(raster_layer$level, "character")
    expect_type(raster_layer$class, "integer")
    expect_type(raster_layer$id, "integer")
    expect_type(raster_layer$x, "double")
    expect_type(raster_layer$y, "double")
})

test_that("get_centroid returns centroid for each patch", {

    raster_layer <- get_centroid(landscape)

    np <- lsm_l_np(landscape)

    expect_true(object = nrow(raster_layer) == np$value)
})

test_that("get_centroid allows to set cell_center", {

    expect_warning(get_centroid(landscape, cell_center = TRUE),
                   regexp = "For some patches several cell centers are returned as centroid.")

    raster_layer <- get_centroid(landscape, cell_center = TRUE)
    np <- lsm_l_np(landscape)

    expect_true(object = nrow(raster_layer) > np$value)
})






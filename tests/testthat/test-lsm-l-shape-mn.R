context("landscape level shape_mn metric")

landscapemetrics_landscape_landscape_shape_mn <- lsm_l_shape_mn(landscape)

test_that("lsm_c_shape_mn is typestable", {
    expect_is(landscapemetrics_landscape_landscape_shape_mn, "tbl_df")
    expect_is(lsm_l_shape_mn(landscape_stack), "tbl_df")
    expect_is(lsm_l_shape_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_shape_mn), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_shape_mn$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_mn$level, "character")
    expect_type(landscapemetrics_landscape_landscape_shape_mn$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_mn$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_mn$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_shape_mn$value, "double")
})



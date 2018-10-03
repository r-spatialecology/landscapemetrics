context("landscape level lsm_l_te metric")

landscapemetrics_landscape_landscape_value <- lsm_l_te(landscape)

test_that("lsm_l_te is typestable", {
    expect_is(lsm_l_te(landscape), "tbl_df")
    expect_is(lsm_l_te(landscape_stack), "tbl_df")
    expect_is(lsm_l_te(landscape_brick), "tbl_df")
    expect_is(lsm_l_te(landscape_list), "tbl_df")
})

test_that("lsm_l_te returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_te returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

test_that("lsm_l_te option count_boundary is working", {
    te_with_boundary <- lsm_l_te(landscape, count_boundary = TRUE)
    te_without_boundary <- lsm_l_te(landscape, count_boundary = FALSE)
    expect_less_than(te_without_boundary$value, te_with_boundary$value)
})

test_that("lsm_l_te can handle raster with different xy resolution", {
    expect_is(lsm_l_te(landscape_diff_res), "tbl_df")
})

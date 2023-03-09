context("landscape level lsm_l_frac_sd metric")

landscapemetrics_landscape_landscape_value <- lsm_l_frac_sd(landscape)

test_that("lsm_l_frac_sd is typestable", {
    expect_is(lsm_l_frac_sd(landscape), "tbl_df")
    expect_is(lsm_l_frac_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_frac_sd(landscape_list), "tbl_df")
})

test_that("lsm_l_frac_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_frac_sd returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

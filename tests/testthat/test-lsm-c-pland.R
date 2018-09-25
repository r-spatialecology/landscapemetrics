context("class level lsm_c_pland metric")

landscapemetrics_class_landscape_value <- lsm_c_pland(landscape)

test_that("lsm_c_pland is typestable", {
    expect_is(lsm_c_pland(landscape), "tbl_df")
    expect_is(lsm_c_pland(landscape_stack), "tbl_df")
    expect_is(lsm_c_pland(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_pland returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_pland returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})


context("class level lsm_c_core_sd metric")

landscapemetrics_class_landscape_value <- lsm_c_core_sd(landscape)

test_that("lsm_c_core_sd is typestable", {
    expect_is(lsm_c_core_sd(landscape), "tbl_df")
    expect_is(lsm_c_core_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_core_sd(landscape_brick), "tbl_df")
    expect_is(lsm_c_core_sd(landscape_list), "tbl_df")
})

test_that("lsm_c_core_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_core_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

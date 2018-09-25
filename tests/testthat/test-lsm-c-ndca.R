context("class level lsm_c_ndca metric")

landscapemetrics_class_landscape_value <- lsm_c_ndca(landscape)

test_that("lsm_c_ndca is typestable", {
    expect_is(lsm_c_ndca(landscape), "tbl_df")
    expect_is(lsm_c_ndca(landscape_stack), "tbl_df")
    expect_is(lsm_c_ndca(landscape_list), "tbl_df")
})

test_that("lsm_c_ndca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_ndca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})


context("class level lsm_c_ca metric")

landscapemetrics_class_landscape_value <- lsm_c_ca(landscape)

test_that("lsm_c_ca is typestable", {
    expect_is(lsm_c_ca(landscape), "tbl_df")
    expect_is(lsm_c_ca(landscape_stack), "tbl_df")
    expect_is(lsm_c_ca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_ca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_ca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})




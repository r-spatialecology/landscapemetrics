context("class level lsm_c_pafrac metric")

landscapemetrics_class_landscape_value <- lsm_c_pafrac(landscape)

test_that("lsm_c_pafrac is typestable", {
    expect_is(lsm_c_pafrac(landscape), "tbl_df")
    expect_is(lsm_c_pafrac(landscape_stack), "tbl_df")
    expect_is(lsm_c_pafrac(landscape_list), "tbl_df")
})

test_that("lsm_c_pafrac returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_pafrac returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})


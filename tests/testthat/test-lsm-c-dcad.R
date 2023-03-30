landscapemetrics_class_landscape_value <- lsm_c_dcad(landscape)

test_that("lsm_c_dcad is typestable", {
    expect_s3_class(lsm_c_dcad(landscape), "tbl_df")
    expect_s3_class(lsm_c_dcad(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_dcad(landscape_list), "tbl_df")
})

test_that("lsm_c_dcad returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_dcad returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})


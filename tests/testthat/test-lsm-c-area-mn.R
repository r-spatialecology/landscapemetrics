landscapemetrics_class_landscape_value <- lsm_c_area_mn(landscape)

test_that("lsm_c_area_mn is typestable", {
    expect_s3_class(lsm_c_area_mn(landscape), "tbl_df")
    expect_s3_class(lsm_c_area_mn(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_area_mn(landscape_list), "tbl_df")
})

test_that("lsm_c_area_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_area_mn returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

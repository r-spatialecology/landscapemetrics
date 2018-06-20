context("class level dcore_sd metric")

landscapemetrics_class_landscape_dcore_sd <- lsm_c_dcore_sd(landscape)

test_that("lsm_c_dcore_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_dcore_sd, "tbl_df")
    expect_is(lsm_c_dcore_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_dcore_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_dcore_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_dcore_sd), 6)
})

test_that("lsm_p_dcore_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_dcore_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_dcore_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_dcore_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_dcore_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_dcore_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_dcore_sd$value, "double")
})



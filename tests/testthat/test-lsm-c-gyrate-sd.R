context("class level gyrate_sd metric")

landscapemetrics_class_landscape_gyrate_sd <- lsm_c_gyrate_sd(landscape)

test_that("lsm_c_gyrate_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_gyrate_sd, "tbl_df")
    expect_is(lsm_c_gyrate_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_gyrate_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_gyrate_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_gyrate_sd), 6)
})

test_that("lsm_p_gyrate_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_gyrate_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_gyrate_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_gyrate_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_gyrate_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_gyrate_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_gyrate_sd$value, "double")
})



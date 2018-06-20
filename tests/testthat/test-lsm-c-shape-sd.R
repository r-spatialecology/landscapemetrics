context("class level shape_sd metric")

landscapemetrics_class_landscape_shape_sd <- lsm_c_shape_sd(landscape)

test_that("lsm_c_shape_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_shape_sd, "tbl_df")
    expect_is(lsm_c_shape_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_shape_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_shape_sd returns the desirshape_sd number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_shape_sd), 6)
})

test_that("lsm_p_shape_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_shape_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_shape_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_shape_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_shape_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_shape_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_shape_sd$value, "double")
})



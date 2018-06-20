context("class level circle_sd metric")

landscapemetrics_class_landscape_circle <- lsm_c_circle_sd(landscape)

test_that("lsm_c_circle_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_circle, "tbl_df")
    expect_is(lsm_c_circle_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_circle_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_circle returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_circle), 6)
})

test_that("lsm_p_circle returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_circle$layer, "integer")
    expect_type(landscapemetrics_class_landscape_circle$level, "character")
    expect_type(landscapemetrics_class_landscape_circle$class, "integer")
    expect_type(landscapemetrics_class_landscape_circle$id, "integer")
    expect_type(landscapemetrics_class_landscape_circle$metric, "character")
    expect_type(landscapemetrics_class_landscape_circle$value, "double")
})



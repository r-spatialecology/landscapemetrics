context("patch level shape metric")

fragstats_patch_landscape_value <- fragstats_patch_landscape$SHAPE
landscapemetrics_patch_landscape_value <- lsm_p_shape(landscape)

test_that("lsm_p_shape results are equal to fragstats", {
    expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
                        round(landscapemetrics_patch_landscape_value$value, 4)))
})

test_that("lsm_p_shape is typestable", {
    expect_is(lsm_p_shape(landscape), "tbl_df")
    expect_is(lsm_p_shape(landscape_stack), "tbl_df")
    expect_is(lsm_p_shape(list(landscape, landscape)), "tbl_df")
    expect_is(lsm_p_shape(landscape_stars), "tbl_df")
})

test_that("lsm_p_shape returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_shape returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})


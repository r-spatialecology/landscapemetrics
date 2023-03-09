context("patch level lsm_p_enn metric")

landscapemetrics_patch_landscape_value <- lsm_p_enn(landscape)

test_that("lsm_p_enn is typestable", {
    expect_is(lsm_p_enn(landscape), "tbl_df")
    expect_is(lsm_p_enn(landscape_stack), "tbl_df")
    expect_is(lsm_p_enn(landscape_list), "tbl_df")
})

test_that("lsm_p_enn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_enn returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})



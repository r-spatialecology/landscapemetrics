context("patch level lsm_p_cai metric")

landscapemetrics_patch_landscape_value <- lsm_p_cai(landscape)

test_that("lsm_p_cai is typestable", {
    expect_is(lsm_p_cai(landscape), "tbl_df")
    expect_is(lsm_p_cai(landscape_stack), "tbl_df")
    expect_is(lsm_p_cai(landscape_brick), "tbl_df")
    expect_is(lsm_p_cai(landscape_list), "tbl_df")
})

test_that("lsm_p_cai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_cai returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

context("patch level cai metric")

landscapemetrics_patch_landscape_cai <- lsm_p_cai(landscape)

test_that("lsm_p_cai is typestable", {
    expect_is(landscapemetrics_patch_landscape_cai, "tbl_df")
    expect_is(lsm_p_cai(landscape_stack), "tbl_df")
    expect_is(lsm_p_cai(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_cai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_cai), 6)
})

test_that("lsm_p_cai returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_cai$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_cai$level, "character")
    expect_type(landscapemetrics_patch_landscape_cai$class, "integer")
    expect_type(landscapemetrics_patch_landscape_cai$id, "integer")
    expect_type(landscapemetrics_patch_landscape_cai$metric, "character")
    expect_type(landscapemetrics_patch_landscape_cai$value, "double")
})

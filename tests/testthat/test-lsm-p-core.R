context("patch level core metric")

landscapemetrics_patch_landscape_core <- lsm_p_core(landscape)

test_that("lsm_p_core is typestable", {
    expect_is(landscapemetrics_patch_landscape_core, "tbl_df")
    expect_is(lsm_p_core(landscape_stack), "tbl_df")
    expect_is(lsm_p_core(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_core returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_core), 6)
})

test_that("lsm_p_core returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_core$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_core$level, "character")
    expect_type(landscapemetrics_patch_landscape_core$class, "integer")
    expect_type(landscapemetrics_patch_landscape_core$id, "integer")
    expect_type(landscapemetrics_patch_landscape_core$metric, "character")
    expect_type(landscapemetrics_patch_landscape_core$value, "double")
})



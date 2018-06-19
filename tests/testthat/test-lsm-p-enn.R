context("patch level enn metric")

fragstats_patch_landscape_enn <- fragstats_patch_landscape$ENN
landscapemetrics_patch_landscape_enn <- lsm_p_enn(landscape)

test_that("lsm_p_enn results are comparable to fragstats", {
    expect_true(all(fragstats_patch_landscape_enn %in%
                        round(landscapemetrics_patch_landscape_enn$value, digits = 4)))
})

test_that("lsm_p_enn is typestable", {
    expect_is(landscapemetrics_patch_landscape_enn, "tbl_df")
    expect_is(lsm_p_enn(landscape_stack), "tbl_df")
    expect_is(lsm_p_enn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_enn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_enn), 6)
})

test_that("lsm_p_enn returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_enn$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_enn$level, "character")
    expect_type(landscapemetrics_patch_landscape_enn$class, "integer")
    expect_type(landscapemetrics_patch_landscape_enn$id, "integer")
    expect_type(landscapemetrics_patch_landscape_enn$metric, "character")
    expect_type(landscapemetrics_patch_landscape_enn$value, "double")
})



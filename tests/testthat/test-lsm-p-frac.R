context("patch level frac metric")

fragstats_patch_landscape_frac <- fragstats_patch_landscape$FRAC
landscapemetrics_patch_landscape_frac <- lsm_p_frac(landscape)

test_that("lsm_p_frac results are comparable to fragstats", {
    expect_true(all(fragstats_patch_landscape_frac %in%
                        round(landscapemetrics_patch_landscape_frac$value, 4)))
})

test_that("lsm_p_frac is typestable", {
    expect_is(landscapemetrics_patch_landscape_frac, "tbl_df")
    expect_is(lsm_p_frac(landscape_stack), "tbl_df")
    expect_is(lsm_p_frac(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_frac returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_frac), 6)
})

test_that("lsm_p_frac returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_frac$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$level, "character")
    expect_type(landscapemetrics_patch_landscape_frac$class, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$id, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$metric, "character")
    expect_type(landscapemetrics_patch_landscape_frac$value, "double")
})



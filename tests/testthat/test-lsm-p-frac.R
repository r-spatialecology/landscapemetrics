context("patch level frac metric")

fragstats_patch_landscape_frac <- fragstats_patch_landscape$FRAC
fragstats_patch_landscapestack_frac <- fragstats_patch_landscapestack$FRAC
fragstats_patch_augusta_nlcd_frac <- fragstats_patch_augusta_nlcd$FRAC
fragstats_patch_podlasie_frac <- fragstats_patch_podlasie$FRAC
landscapemetrics_patch_landscape_frac <- lsm_p_frac(landscape)
landscapemetrics_patch_landscape_stack_frac <- lsm_p_frac(landscape_stack)
landscapemetrics_patch_augusta_nlcd_frac <- lsm_p_frac(augusta_nlcd)
landscapemetrics_patch_podlasie_ccilc_frac <- lsm_p_frac(podlasie_ccilc)

test_that("lsm_p_frac results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_frac %in%
                        landscapemetrics_patch_landscape_frac$value))
    expect_true(all(fragstats_patch_landscapestack_frac %in%
                        landscapemetrics_patch_landscape_stack_frac$value))
    expect_true(all(fragstats_patch_augusta_nlcd_frac %in%
                        landscapemetrics_patch_augusta_nlcd_frac$value))
})

test_that("lsm_p_frac is typestable", {
    expect_is(lsm_p_frac(landscape), "tbl_df")
    expect_is(lsm_p_frac(landscape_stack), "tbl_df")
    expect_is(lsm_p_frac(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_frac returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_values), 6)
})

test_that("lsm_p_frac returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_frac$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$level, "character")
    expect_type(landscapemetrics_patch_landscape_frac$class, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$id, "integer")
    expect_type(landscapemetrics_patch_landscape_frac$metric, "character")
    expect_type(landscapemetrics_patch_landscape_frac$value, "double")
})



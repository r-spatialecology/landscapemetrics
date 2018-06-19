context("patch level gyrate metric")

fragstats_patch_landscape_gyrate <- fragstats_patch_landscape$GYRATE
fragstats_patch_landscapestack_gyrate <- fragstats_patch_landscapestack$GYRATE
fragstats_patch_augusta_nlcd_gyrate <- fragstats_patch_augusta_nlcd$GYRATE
fragstats_patch_podlasie_gyrate <- fragstats_patch_podlasie$GYRATE
landscapemetrics_patch_landscape_gyrate <- lsm_p_gyrate(landscape)
landscapemetrics_patch_landscape_stack_gyrate <- lsm_p_gyrate(landscape_stack)
landscapemetrics_patch_augusta_nlcd_gyrate <- lsm_p_gyrate(augusta_nlcd)
landscapemetrics_patch_podlasie_ccilc_gyrate <- lsm_p_gyrate(podlasie_ccilc)

test_that("lsm_p_gyrate results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_gyrate %in%
                        landscapemetrics_patch_landscape_gyrate$value))
    expect_true(all(fragstats_patch_landscapestack_gyrate %in%
                        landscapemetrics_patch_landscape_stack_gyrate$value))
    expect_true(all(fragstats_patch_augusta_nlcd_gyrate %in%
                        landscapemetrics_patch_augusta_nlcd_gyrate$value))
})

test_that("lsm_p_gyrate is typestable", {
    expect_is(lsm_p_gyrate(landscape), "tbl_df")
    expect_is(lsm_p_gyrate(landscape_stack), "tbl_df")
    expect_is(lsm_p_gyrate(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_gyrate returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_values), 6)
})

test_that("lsm_p_gyrate returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_gyrate$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$level, "character")
    expect_type(landscapemetrics_patch_landscape_gyrate$class, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$id, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$metric, "character")
    expect_type(landscapemetrics_patch_landscape_gyrate$value, "double")
})



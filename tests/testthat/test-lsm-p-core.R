context("patch level core metric")

fragstats_patch_landscape_core <- fragstats_patch_landscape$CORE
fragstats_patch_landscapestack_core <- fragstats_patch_landscapestack$CORE
fragstats_patch_augusta_nlcd_core <- fragstats_patch_augusta_nlcd$CORE
fragstats_patch_podlasie_core <- fragstats_patch_podlasie$CORE
landscapemetrics_patch_landscape_core <- lsm_p_core(landscape)
landscapemetrics_patch_landscape_stack_core <- lsm_p_core(landscape_stack)
landscapemetrics_patch_augusta_nlcd_core <- lsm_p_core(augusta_nlcd)
landscapemetrics_patch_podlasie_ccilc_core <- lsm_p_core(podlasie_ccilc)

test_that("lsm_p_core results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_core %in%
                        landscapemetrics_patch_landscape_core$value))
    expect_true(all(fragstats_patch_landscapestack_core %in%
                        landscapemetrics_patch_landscape_stack_core$value))
    expect_true(all(fragstats_patch_augusta_nlcd_core %in%
                        landscapemetrics_patch_augusta_nlcd_core$value))
})

test_that("lsm_p_core is typestable", {
    expect_is(lsm_p_core(landscape), "tbl_df")
    expect_is(lsm_p_core(landscape_stack), "tbl_df")
    expect_is(lsm_p_core(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_core returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_values), 6)
})

test_that("lsm_p_core returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_core$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_core$level, "character")
    expect_type(landscapemetrics_patch_landscape_core$class, "integer")
    expect_type(landscapemetrics_patch_landscape_core$id, "integer")
    expect_type(landscapemetrics_patch_landscape_core$metric, "character")
    expect_type(landscapemetrics_patch_landscape_core$value, "double")
})



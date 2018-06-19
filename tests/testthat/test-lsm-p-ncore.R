context("patch level ncore metric")

fragstats_patch_landscape_ncore <- fragstats_patch_landscape$NCORE
# fragstats_patch_landscapestack_ncore <- fragstats_patch_landscapestack$NCORE
# fragstats_patch_augusta_nlcd_ncore <- fragstats_patch_augusta_nlcd$NCORE
# fragstats_patch_podlasie_ncore <- fragstats_patch_podlasie$NCORE
landscapemetrics_patch_landscape_ncore <- lsm_p_ncore(landscape)
# landscapemetrics_patch_landscape_stack_ncore <- lsm_p_ncore(landscape_stack)
# landscapemetrics_patch_augusta_nlcd_ncore <- lsm_p_ncore(augusta_nlcd)
# landscapemetrics_patch_podlasie_ccilc_ncore <- lsm_p_ncore(podlasie_ccilc)

test_that("lsm_p_ncore results are equal to fragstats", {
    expect_true(all(round(sort(fragstats_patch_landscape_ncore) / sort(landscapemetrics_patch_landscape_ncore$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_landscapestack_ncore) / sort(landscapemetrics_patch_landscape_stack_ncore$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_augusta_nlcd_ncore) / sort(landscapemetrics_patch_augusta_nlcd_ncore$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_podlasie_ncore) / sort(landscapemetrics_patch_podlasie_ccilc_ncore$value) *100) - 100 > 10))
})

test_that("lsm_p_ncore is typestable", {
    expect_is(lsm_p_ncore(landscape), "tbl_df")
    expect_is(lsm_p_ncore(landscape_stack), "tbl_df")
    expect_is(lsm_p_ncore(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_ncore returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_ncore), 6)
})

test_that("lsm_p_ncore returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_ncore$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_ncore$level, "character")
    expect_type(landscapemetrics_patch_landscape_ncore$class, "integer")
    expect_type(landscapemetrics_patch_landscape_ncore$id, "integer")
    expect_type(landscapemetrics_patch_landscape_ncore$metric, "character")
    expect_type(landscapemetrics_patch_landscape_ncore$value, "double")
})



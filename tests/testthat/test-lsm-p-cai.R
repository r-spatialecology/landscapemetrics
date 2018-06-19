context("patch level cai metric")

fragstats_patch_landscape_cai <- fragstats_patch_landscape$CAI
# fragstats_patch_landscapestack_cai <- fragstats_patch_landscapestack$CAI
# fragstats_patch_augusta_nlcd_cai <- fragstats_patch_augusta_nlcd$CAI
# fragstats_patch_podlasie_cai <- fragstats_patch_podlasie$CAI
landscapemetrics_patch_landscape_cai <- lsm_p_cai(landscape)
# landscapemetrics_patch_landscape_stack_cai <- lsm_p_cai(landscape_stack)
# landscapemetrics_patch_augusta_nlcd_cai <- lsm_p_cai(augusta_nlcd)
# landscapemetrics_patch_podlasie_ccilc_cai <- lsm_p_cai(podlasie_ccilc)

test_that("lsm_p_cai results are equal to fragstats", {
    expect_true(all(round(sort(fragstats_patch_landscape_cai) / sort(landscapemetrics_patch_landscape_cai$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_landscapestack_cai) / sort(landscapemetrics_patch_landscape_stack_cai$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_augusta_nlcd_cai) / sort(landscapemetrics_patch_augusta_nlcd_cai$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_podlasie_cai) / sort(landscapemetrics_patch_podlasie_ccilc_cai$value) *100) - 100 > 10))
})

test_that("lsm_p_cai is typestable", {
    expect_is(lsm_p_cai(landscape), "tbl_df")
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

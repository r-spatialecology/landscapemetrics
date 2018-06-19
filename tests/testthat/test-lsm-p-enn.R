context("patch level enn metric")

fragstats_patch_landscape_enn <- fragstats_patch_landscape$ENN
# fragstats_patch_landscapestack_enn <- fragstats_patch_landscapestack$ENN
# fragstats_patch_augusta_nlcd_enn <- fragstats_patch_augusta_nlcd$ENN
# fragstats_patch_podlasie_enn <- fragstats_patch_podlasie$ENN
landscapemetrics_patch_landscape_enn <- lsm_p_enn(landscape)
# landscapemetrics_patch_landscape_stack_enn <- lsm_p_enn(landscape_stack)
# landscapemetrics_patch_augusta_nlcd_enn <- lsm_p_enn(augusta_nlcd)
# landscapemetrics_patch_podlasie_ccilc_enn <- lsm_p_enn(podlasie_ccilc)

test_that("lsm_p_enn results are comparable to fragstats", {
    expect_true(all(round(sort(fragstats_patch_landscape_enn) / sort(landscapemetrics_patch_landscape_enn$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_landscapestack_enn) / sort(landscapemetrics_patch_landscape_stack_enn$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_augusta_nlcd_enn) / sort(landscapemetrics_patch_augusta_nlcd_enn$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_podlasie_enn) / sort(landscapemetrics_patch_podlasie_ccilc_enn$value) *100) - 100 > 10))
})

test_that("lsm_p_enn is typestable", {
    expect_is(lsm_p_enn(landscape), "tbl_df")
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



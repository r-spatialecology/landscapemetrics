context("patch level circle metric")

fragstats_patch_landscape_circle <- fragstats_patch_landscape$CIRCLE
# fragstats_patch_landscapestack_circle <- fragstats_patch_landscapestack$CIRCLE
# fragstats_patch_augusta_nlcd_circle <- fragstats_patch_augusta_nlcd$CIRCLE
# fragstats_patch_podlasie_circle <- fragstats_patch_podlasie$CIRCLE
landscapemetrics_patch_landscape_circle <- lsm_p_circle(landscape)
# landscapemetrics_patch_landscape_stack_circle <- lsm_p_circle(landscape_stack)
# landscapemetrics_patch_augusta_nlcd_circle <- lsm_p_circle(augusta_nlcd)
# landscapemetrics_patch_podlasie_ccilc_circle <- lsm_p_circle(podlasie_ccilc)

test_that("lsm_p_circle results are comparable to fragstats", {
    expect_true(all(round(sort(fragstats_patch_landscape_circle) / sort(landscapemetrics_patch_landscape_circle$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_landscapestack_circle) / sort(landscapemetrics_patch_landscape_stack_circle$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_augusta_nlcd_circle) / sort(landscapemetrics_patch_augusta_nlcd_circle$value) *100) - 100 > 10))
    # expect_true(all(round(sort(fragstats_patch_podlasie_circle) / sort(landscapemetrics_patch_podlasie_ccilc_circle$value) *100) - 100 > 10))
})

test_that("lsm_p_circle is typestable", {
    expect_is(lsm_p_circle(landscape), "tbl_df")
    expect_is(lsm_p_circle(landscape_stack), "tbl_df")
    expect_is(lsm_p_circle(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_circle returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_circle), 6)
})

test_that("lsm_p_circle returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_circle$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_circle$level, "character")
    expect_type(landscapemetrics_patch_landscape_circle$class, "integer")
    expect_type(landscapemetrics_patch_landscape_circle$id, "integer")
    expect_type(landscapemetrics_patch_landscape_circle$metric, "character")
    expect_type(landscapemetrics_patch_landscape_circle$value, "double")
})

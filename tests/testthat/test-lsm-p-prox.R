# context("patch level prox metric")
#
# fragstats_patch_landscape_prox <- fragstats_patch_landscape$PROX
# fragstats_patch_landscapestack_prox <- fragstats_patch_landscapestack$PROX
# fragstats_patch_augusta_nlcd_prox <- fragstats_patch_augusta_nlcd$PROX
# fragstats_patch_podlasie_prox <- fragstats_patch_podlasie$PROX
# landscapemetrics_patch_landscape_prox <- lsm_p_prox(landscape)
# landscapemetrics_patch_landscape_stack_prox <- lsm_p_prox(landscape_stack)
# landscapemetrics_patch_augusta_nlcd_prox <- lsm_p_prox(augusta_nlcd)
# landscapemetrics_patch_podlasie_ccilc_prox <- lsm_p_prox(podlasie_ccilc)
#
# test_that("lsm_p_prox results are equal to fragstats", {
#     expect_true(all(round(sort(fragstats_patch_landscape_prox) / sort(landscapemetrics_patch_landscape_prox$value) *100) - 100 > 10))
#     expect_true(all(round(sort(fragstats_patch_landscapestack_prox) / sort(landscapemetrics_patch_landscape_stack_prox$value) *100) - 100 > 10))
#     expect_true(all(round(sort(fragstats_patch_augusta_nlcd_prox) / sort(landscapemetrics_patch_augusta_nlcd_prox$value) *100) - 100 > 10))
#     expect_true(all(round(sort(fragstats_patch_podlasie_prox) / sort(landscapemetrics_patch_podlasie_ccilc_prox$value) *100) - 100 > 10))
# })
#
# test_that("lsm_p_prox is typestable", {
#     expect_is(lsm_p_prox(landscape), "tbl_df")
#     expect_is(lsm_p_prox(landscape_stack), "tbl_df")
#     expect_is(lsm_p_prox(list(landscape, landscape)), "tbl_df")
# })
#
# test_that("lsm_p_prox returns the desired number of columns", {
#     expect_equal(ncol(landscapemetrics_values), 6)
# })
#
# test_that("lsm_p_prox returns in every column the correct type", {
#     expect_type(landscapemetrics_patch_landscape_prox$layer, "integer")
#     expect_type(landscapemetrics_patch_landscape_prox$level, "character")
#     expect_type(landscapemetrics_patch_landscape_prox$class, "integer")
#     expect_type(landscapemetrics_patch_landscape_prox$id, "integer")
#     expect_type(landscapemetrics_patch_landscape_prox$metric, "character")
#     expect_type(landscapemetrics_patch_landscape_prox$value, "double")
# })
#

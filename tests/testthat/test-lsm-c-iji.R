# context("class level iji metric")
#
# fragstats_class_landscape_iji <- fragstats_class_landscape$IJI
# landscapemetrics_class_landscape_iji <- lsm_c_iji(landscape)
#
# test_that("lsm_c_iji results are equal to fragstats", {
#     expect_true(all(fragstats_class_landscape_iji %in%
#                         round(landscapemetrics_class_landscape_iji$value, 4)))
# })
#
# test_that("lsm_c_iji is typestable", {
#     expect_is(landscapemetrics_class_landscape_iji, "tbl_df")
#     expect_is(lsm_c_iji(landscape_stack), "tbl_df")
#     expect_is(lsm_c_iji(list(landscape, landscape)), "tbl_df")
# })
#
# test_that("lsm_c_iji returns the desired number of columns", {
#     expect_equal(ncol(landscapemetrics_class_landscape_iji), 6)
# })
#
# test_that("lsm_c_iji returns in every column the correct type", {
#     expect_type(landscapemetrics_class_landscape_iji$layer, "integer")
#     expect_type(landscapemetrics_class_landscape_iji$level, "character")
#     expect_type(landscapemetrics_class_landscape_iji$class, "integer")
#     expect_type(landscapemetrics_class_landscape_iji$id, "integer")
#     expect_type(landscapemetrics_class_landscape_iji$metric, "character")
#     expect_type(landscapemetrics_class_landscape_iji$value, "double")
# })
#
#

context("patch level core metric")

# fragstats_patch_landscape_core <- fragstats_patch_landscape$CORE
landscapemetrics_patch_landscape_core <- lsm_p_core(landscape)
#
# test_that("lsm_p_core results are equal to fragstats", {
#     expect_true(all(fragstats_patch_landscape_core %in%
#                         round(landscapemetrics_patch_landscape_core$value, 4)))
# })

test_that("lsm_p_core is typestable", {
    expect_is(landscapemetrics_patch_landscape_core, "tbl_df")
    expect_is(lsm_p_core(landscape_stack), "tbl_df")
    expect_is(lsm_p_core(list(landscape, landscape)), "tbl_df")
    expect_is(lsm_p_core(landscape_stars), "tbl_df")
})

test_that("lsm_p_core returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_core), 6)
})

test_that("lsm_p_core returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_core$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_core$level, "character")
    expect_type(landscapemetrics_patch_landscape_core$class, "integer")
    expect_type(landscapemetrics_patch_landscape_core$id, "integer")
    expect_type(landscapemetrics_patch_landscape_core$metric, "character")
    expect_type(landscapemetrics_patch_landscape_core$value, "double")
})



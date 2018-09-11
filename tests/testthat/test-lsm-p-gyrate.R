context("patch level gyrate metric")

# fragstats_patch_landscape_gyrate <- fragstats_patch_landscape$GYRATE
landscapemetrics_patch_landscape_gyrate <- lsm_p_gyrate(landscape)
#
# test_that("lsm_p_gyrate results are comparable to fragstats", {
#     expect_true(all(fragstats_patch_landscape_gyrate %in%
#                         round(landscapemetrics_patch_landscape_gyrate$value, 4)))
# })

test_that("lsm_p_gyrate is typestable", {
    expect_is(landscapemetrics_patch_landscape_gyrate, "tbl_df")
    expect_is(lsm_p_gyrate(landscape_stack), "tbl_df")
    expect_is(lsm_p_gyrate(list(landscape, landscape)), "tbl_df")
    expect_is(lsm_p_gyrate(landscape_stars), "tbl_df")
})

test_that("lsm_p_gyrate returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_gyrate), 6)
})

test_that("lsm_p_gyrate returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_gyrate$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$level, "character")
    expect_type(landscapemetrics_patch_landscape_gyrate$class, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$id, "integer")
    expect_type(landscapemetrics_patch_landscape_gyrate$metric, "character")
    expect_type(landscapemetrics_patch_landscape_gyrate$value, "double")
})



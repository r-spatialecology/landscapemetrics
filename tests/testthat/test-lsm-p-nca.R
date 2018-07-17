context("patch level nca metric")
#
# fragstats_patch_landscape_nca <- fragstats_patch_landscape$nca
landscapemetrics_patch_landscape_nca <- lsm_p_nca(landscape)

# test_that("lsm_p_nca results are comparable to fragstats", {
#     expect_true(all(fragstats_patch_landscape_nca %in%
#                         round(landscapemetrics_patch_landscape_nca$value, 4)))
# })

test_that("lsm_p_nca is typestable", {
    expect_is(landscapemetrics_patch_landscape_nca, "tbl_df")
    expect_is(lsm_p_nca(landscape_stack), "tbl_df")
    expect_is(lsm_p_nca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_nca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_nca), 6)
})

test_that("lsm_p_nca returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_nca$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_nca$level, "character")
    expect_type(landscapemetrics_patch_landscape_nca$class, "integer")
    expect_type(landscapemetrics_patch_landscape_nca$id, "integer")
    expect_type(landscapemetrics_patch_landscape_nca$metric, "character")
    expect_type(landscapemetrics_patch_landscape_nca$value, "double")
})



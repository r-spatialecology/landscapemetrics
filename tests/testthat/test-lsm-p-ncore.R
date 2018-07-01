context("patch level ncore metric")
#
# fragstats_patch_landscape_ncore <- fragstats_patch_landscape$NCORE
landscapemetrics_patch_landscape_ncore <- lsm_p_ncore(landscape)

# test_that("lsm_p_ncore results are comparable to fragstats", {
#     expect_true(all(fragstats_patch_landscape_ncore %in%
#                         round(landscapemetrics_patch_landscape_ncore$value, 4)))
# })

test_that("lsm_p_ncore is typestable", {
    expect_is(landscapemetrics_patch_landscape_ncore, "tbl_df")
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



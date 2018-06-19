context("patch level perim metric")

fragstats_patch_landscape_perim <- fragstats_patch_landscape$PERIM
landscapemetrics_patch_landscape_perim <- lsm_p_perim(landscape)

test_that("lsm_p_perim results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_perim %in%
                        landscapemetrics_patch_landscape_perim$value))
})

test_that("lsm_p_perim is typestable", {
    expect_is(landscapemetrics_patch_landscape_perim, "tbl_df")
    expect_is(lsm_p_perim(landscape_stack), "tbl_df")
    expect_is(lsm_p_perim(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_perim returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_perim), 6)
})

test_that("lsm_p_perim returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_perim$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_perim$level, "character")
    expect_type(landscapemetrics_patch_landscape_perim$class, "integer")
    expect_type(landscapemetrics_patch_landscape_perim$id, "integer")
    expect_type(landscapemetrics_patch_landscape_perim$metric, "character")
    expect_type(landscapemetrics_patch_landscape_perim$value, "double")
})



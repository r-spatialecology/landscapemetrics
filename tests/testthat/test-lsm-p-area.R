context("patch level area metric")

fragstats_patch_landscape_area <- fragstats_patch_landscape$AREA
landscapemetrics_patch_landscape_area <- lsm_p_area(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_area %in%
                        landscapemetrics_patch_landscape_area$value))
})

test_that("lsm_p_area is typestable", {
    expect_is(landscapemetrics_patch_landscape_area, "tbl_df")
    expect_is(lsm_p_area(landscape_stack), "tbl_df")
    expect_is(lsm_p_area(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_area), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_area$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_area$level, "character")
    expect_type(landscapemetrics_patch_landscape_area$class, "integer")
    expect_type(landscapemetrics_patch_landscape_area$id, "integer")
    expect_type(landscapemetrics_patch_landscape_area$metric, "character")
    expect_type(landscapemetrics_patch_landscape_area$value, "double")
})



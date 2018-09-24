context("patch level lsm_p_area metric")

fragstats_patch_landscape_value <- fragstats_patch_landscape$AREA
landscapemetrics_patch_landscape_value <- lsm_p_area(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
                        round(landscapemetrics_patch_landscape_value$value, 4)))
})

test_that("lsm_p_area is typestable", {
    expect_is(lsm_p_area(landscape), "tbl_df")
    expect_is(lsm_p_area(landscape_stack), "tbl_df")
    expect_is(lsm_p_area(list(landscape, landscape)), "tbl_df")
    # expect_is(lsm_p_area(landscape_stars), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})



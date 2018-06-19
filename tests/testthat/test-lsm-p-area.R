context("patch level area metric")

fragstats_patch_landscape_area <- fragstats_patch_landscape$AREA
fragstats_patch_landscapestack_area <- fragstats_patch_landscapestack$AREA
fragstats_patch_augusta_nlcd_area <- fragstats_patch_augusta_nlcd$AREA
fragstats_patch_podlasie_area <- fragstats_patch_podlasie$AREA
landscapemetrics_patch_landscape_area <- lsm_p_area(landscape)
landscapemetrics_patch_landscape_stack_area <- lsm_p_area(landscape_stack)
landscapemetrics_patch_augusta_nlcd_area <- lsm_p_area(augusta_nlcd)
landscapemetrics_patch_podlasie_ccilc_area <- lsm_p_area(podlasie_ccilc)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_area %in%
                        landscapemetrics_patch_landscape_area$value))
    expect_true(all(fragstats_patch_landscapestack_area %in%
                        landscapemetrics_patch_landscape_stack_area$value))
    expect_true(all(fragstats_patch_augusta_nlcd_area %in%
                        landscapemetrics_patch_augusta_nlcd_area$value))
    expect_true(all(fragstats_patch_podlasie_area %in%
                        landscapemetrics_patch_podlasie_ccilc_area$value))
})

test_that("lsm_p_area is typestable", {
    expect_is(lsm_p_area(landscape), "tbl_df")
    expect_is(lsm_p_area(landscape_stack), "tbl_df")
    expect_is(lsm_p_area(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_values), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_area$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_area$level, "character")
    expect_type(landscapemetrics_patch_landscape_area$class, "integer")
    expect_type(landscapemetrics_patch_landscape_area$id, "integer")
    expect_type(landscapemetrics_patch_landscape_area$metric, "character")
    expect_type(landscapemetrics_patch_landscape_area$value, "double")
})



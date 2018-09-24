context("patch level lsm_p_para metric")

fragstats_patch_landscape_value <- fragstats_patch_landscape$PARA
landscapemetrics_patch_landscape_value <- lsm_p_para(landscape)

test_that("lsm_p_para results are equal to fragstats", {
    expect_true(all(round(fragstats_patch_landscape_value, 4) %in%
                        round(landscapemetrics_patch_landscape_value$value, 4)))
})

test_that("lsm_p_para is typestable", {
    expect_is(lsm_p_para(landscape), "tbl_df")
    expect_is(lsm_p_para(landscape_stack), "tbl_df")
    expect_is(lsm_p_para(list(landscape, landscape)), "tbl_df")
    # expect_is(lsm_p_para(landscape_stars), "tbl_df")
})

test_that("lsm_p_para returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_para returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})



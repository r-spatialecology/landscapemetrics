context("patch level para metric")

fragstats_patch_landscape_para <- fragstats_patch_landscape$PARA
landscapemetrics_patch_landscape_para <- lsm_p_para(landscape)

test_that("lsm_p_para results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_para %in%
                        round((landscapemetrics_patch_landscape_para$value * 10000), 4)))
})

test_that("lsm_p_para is typestable", {
    expect_is(landscapemetrics_patch_landscape_para, "tbl_df")
    expect_is(lsm_p_para(landscape_stack), "tbl_df")
    expect_is(lsm_p_para(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_para returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_para), 6)
})

test_that("lsm_p_para returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_para$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_para$level, "character")
    expect_type(landscapemetrics_patch_landscape_para$class, "integer")
    expect_type(landscapemetrics_patch_landscape_para$id, "integer")
    expect_type(landscapemetrics_patch_landscape_para$metric, "character")
    expect_type(landscapemetrics_patch_landscape_para$value, "double")
})



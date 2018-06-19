context("patch level para metric")

fragstats_patch_landscape_para <- fragstats_patch_landscape$PARA
fragstats_patch_landscapestack_para <- fragstats_patch_landscapestack$PARA
fragstats_patch_augusta_nlcd_para <- fragstats_patch_augusta_nlcd$PARA
fragstats_patch_podlasie_para <- fragstats_patch_podlasie$PARA
landscapemetrics_patch_landscape_para <- lsm_p_para(landscape)
landscapemetrics_patch_landscape_stack_para <- lsm_p_para(landscape_stack)
landscapemetrics_patch_augusta_nlcd_para <- lsm_p_para(augusta_nlcd)
landscapemetrics_patch_podlasie_ccilc_para <- lsm_p_para(podlasie_ccilc)

test_that("lsm_p_para results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_para %in%
                        landscapemetrics_patch_landscape_para$value))
    expect_true(all(fragstats_patch_landscapestack_para %in%
                        landscapemetrics_patch_landscape_stack_para$value))
    expect_true(all(fragstats_patch_augusta_nlcd_para %in%
                        landscapemetrics_patch_augusta_nlcd_para$value))
})

test_that("lsm_p_para is typestable", {
    expect_is(lsm_p_para(landscape), "tbl_df")
    expect_is(lsm_p_para(landscape_stack), "tbl_df")
    expect_is(lsm_p_para(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_para returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_values), 6)
})

test_that("lsm_p_para returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_para$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_para$level, "character")
    expect_type(landscapemetrics_patch_landscape_para$class, "integer")
    expect_type(landscapemetrics_patch_landscape_para$id, "integer")
    expect_type(landscapemetrics_patch_landscape_para$metric, "character")
    expect_type(landscapemetrics_patch_landscape_para$value, "double")
})



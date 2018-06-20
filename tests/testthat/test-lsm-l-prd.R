context("landscape level prd metric")

fragstats_landscape_landscape_prd <- fragstats_landscape_landscape$PRD
landscapemetrics_landscape_landscape_prd <- lsm_l_prd(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_prd %in%
                        round(landscapemetrics_landscape_landscape_prd$value,4)))
})

test_that("lsm_c_prd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_prd, "tbl_df")
    expect_is(lsm_l_prd(landscape_stack), "tbl_df")
    expect_is(lsm_l_prd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_prd), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_prd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_prd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_prd$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_prd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_prd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_prd$value, "double")
})



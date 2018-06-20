context("landscape level frac_mn metric")

fragstats_landscape_landscape_frac_mn <- fragstats_landscape_landscape$FRAC_MN
landscapemetrics_landscape_landscape_frac_mn <- lsm_l_frac_mn(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_frac_mn %in%
                        round(landscapemetrics_landscape_landscape_frac_mn$value,4)))
})

test_that("lsm_c_frac_mn is typestable", {
    expect_is(landscapemetrics_landscape_landscape_frac_mn, "tbl_df")
    expect_is(lsm_l_frac_mn(landscape_stack), "tbl_df")
    expect_is(lsm_l_frac_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_frac_mn), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_frac_mn$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_frac_mn$level, "character")
    expect_type(landscapemetrics_landscape_landscape_frac_mn$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_frac_mn$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_frac_mn$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_frac_mn$value, "double")
})



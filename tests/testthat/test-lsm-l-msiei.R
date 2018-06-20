context("landscape level msiei metric")

fragstats_landscape_landscape_msiei <- fragstats_landscape_landscape$MSIEI
landscapemetrics_landscape_landscape_msiei <- lsm_l_msiei(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_msiei %in%
                        round(landscapemetrics_landscape_landscape_msiei$value,4)))
})

test_that("lsm_c_msiei is typestable", {
    expect_is(landscapemetrics_landscape_landscape_msiei, "tbl_df")
    expect_is(lsm_l_msiei(landscape_stack), "tbl_df")
    expect_is(lsm_l_msiei(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_msiei), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_msiei$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_msiei$level, "character")
    expect_type(landscapemetrics_landscape_landscape_msiei$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_msiei$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_msiei$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_msiei$value, "double")
})



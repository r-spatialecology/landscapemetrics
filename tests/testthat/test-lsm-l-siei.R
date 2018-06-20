context("landscape level siei metric")

fragstats_landscape_landscape_siei <- fragstats_landscape_landscape$SIEI
landscapemetrics_landscape_landscape_siei <- lsm_l_siei(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_siei %in%
                        round(landscapemetrics_landscape_landscape_siei$value,4)))
})

test_that("lsm_c_siei is typestable", {
    expect_is(landscapemetrics_landscape_landscape_siei, "tbl_df")
    expect_is(lsm_l_siei(landscape_stack), "tbl_df")
    expect_is(lsm_l_siei(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_siei), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_siei$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_siei$level, "character")
    expect_type(landscapemetrics_landscape_landscape_siei$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_siei$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_siei$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_siei$value, "double")
})

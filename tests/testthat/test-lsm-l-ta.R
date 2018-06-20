context("landscape level ta metric")

fragstats_landscape_landscape_ta <- fragstats_landscape_landscape$TA
landscapemetrics_landscape_landscape_ta <- lsm_l_ta(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_ta %in%
                        round(landscapemetrics_landscape_landscape_ta$value,4)))
})

test_that("lsm_c_ta is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ta, "tbl_df")
    expect_is(lsm_l_ta(landscape_stack), "tbl_df")
    expect_is(lsm_l_ta(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ta), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ta$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ta$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ta$value, "double")
})

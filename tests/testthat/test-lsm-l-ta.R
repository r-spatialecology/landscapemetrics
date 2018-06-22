context("landscape level ta metric")

fragstats_landscape_landscape_ta <- fragstats_landscape_landscape$TA
landscapemetrics_landscape_landscape_ta <- lsm_l_ta(landscape)

test_that("lsm_l_ta results are equal to fragstats", {
    expect_equal(landscapemetrics_landscape_landscape_ta$value,
                 fragstats_landscape_landscape_ta)
})

test_that("lsm_l_ta is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ta, "tbl_df")
    expect_is(lsm_l_ta(landscape_stack), "tbl_df")
    expect_is(lsm_l_ta(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ta returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ta), 6)
})

test_that("lsm_l_ta returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ta$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ta$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ta$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ta$value, "double")
})

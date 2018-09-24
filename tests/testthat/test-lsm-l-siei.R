context("landscape level lsm_l_siei metric")

fragstats_landscape_landscape_value <- fragstats_landscape_landscape$SIEI
landscapemetrics_landscape_landscape_value <- lsm_l_siei(landscape)

test_that("lsm_l_siei results are equal to fragstats", {
    expect_true(round(fragstats_landscape_landscape_value, 4) ==
                    round(landscapemetrics_landscape_landscape_value$value, 4))
})

test_that("lsm_l_siei is typestable", {
    expect_is(lsm_l_siei(landscape), "tbl_df")
    expect_is(lsm_l_siei(landscape_stack), "tbl_df")
    expect_is(lsm_l_siei(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_siei returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_siei returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})


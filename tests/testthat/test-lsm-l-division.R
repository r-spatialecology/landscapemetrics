context("landscape level division metric")

fragstats_landscape_landscape_division <- fragstats_landscape_landscape$DIVISION
landscapemetrics_landscape_landscape_division <- lsm_l_division(landscape)

test_that("lsm_l_division results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_division %in%
                        round(landscapemetrics_landscape_landscape_division$value, 4)))
})

test_that("lsm_l_division is typestable", {
    expect_is(landscapemetrics_landscape_landscape_division, "tbl_df")
    expect_is(lsm_l_division(landscape_stack), "tbl_df")
    expect_is(lsm_l_division(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_division returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_division), 6)
})

test_that("lsm_l_division returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_division$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_division$level, "character")
    expect_type(landscapemetrics_landscape_landscape_division$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_division$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_division$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_division$value, "double")
})



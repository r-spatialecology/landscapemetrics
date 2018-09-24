context("landscape level _lsm_l_dcad metric")

fragstats_landscape_landscape_value <- fragstats_landscape_landscape$DCAD
landscapemetrics_landscape_landscape_value <- lsm_l_dcad(landscape)

# See https://r-spatialecology.github.io/landscapemetrics/
# test_that("lsm_l_dcad results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })

test_that("lsm_l_dcad is typestable", {
    expect_is(lsm_l_dcad(landscape), "tbl_df")
    expect_is(lsm_l_dcad(landscape_stack), "tbl_df")
    expect_is(lsm_l_dcad(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_dcad returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_dcad returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})


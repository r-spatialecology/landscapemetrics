context("landscape level lsm_l_ndca metric")

fragstats_landscape_landscape_value <- fragstats_landscape_landscape$NDCA
landscapemetrics_landscape_landscape_value <- lsm_l_ndca(landscape)

# See https://r-spatialecology.github.io/landscapemetrics/articles/articles/comparing_fragstats_landscapemetrics.html
# test_that("lsm_l_ndca results are equal to fragstats", {
#     expect_true(round(fragstats_landscape_landscape_value, 4) ==
#                     round(landscapemetrics_landscape_landscape_value$value, 4))
# })

test_that("lsm_l_ndca is typestable", {
    expect_is(lsm_l_ndca(landscape), "tbl_df")
    expect_is(lsm_l_ndca(landscape_stack), "tbl_df")
    expect_is(lsm_l_ndca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ndca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_ndca returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})


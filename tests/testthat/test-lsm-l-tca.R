context("landscape level tca metric")

fragstats_landscape_landscape_tca <- fragstats_landscape_landscape$TCA
landscapemetrics_landscape_landscape_tca <- lsm_l_tca(landscape)

test_that("lsm_l_tca results are equal to fragstats", {
    expect_equal(landscapemetrics_landscape_landscape_tca$value,
                 fragstats_landscape_landscape_tca)
})

test_that("lsm_c_tca is typestable", {
    expect_is(landscapemetrics_landscape_landscape_tca, "tbl_df")
    expect_is(lsm_l_tca(landscape_stack), "tbl_df")
    expect_is(lsm_l_tca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_tca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_tca), 6)
})

test_that("lsm_l_tca returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_tca$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_tca$level, "character")
    expect_type(landscapemetrics_landscape_landscape_tca$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_tca$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_tca$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_tca$value, "double")
})

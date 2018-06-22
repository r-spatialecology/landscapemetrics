context("landscape level gyrate_mn metric")

fragstats_landscape_landscape_gyrate_mn <- fragstats_landscape_landscape$gyrate_mn
landscapemetrics_landscape_landscape_gyrate_mn <- lsm_l_gyrate_mn(landscape)

test_that("lsm_l_gyrate_mn results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_gyrate_mn %in%
                        round(landscapemetrics_landscape_landscape_gyrate_mn$value, 4)))
})

test_that("lsm_l_gyrate_mn is typestable", {
    expect_is(landscapemetrics_landscape_landscape_gyrate_mn, "tbl_df")
    expect_is(lsm_l_gyrate_mn(landscape_stack), "tbl_df")
    expect_is(lsm_l_gyrate_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_gyrate_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_gyrate_mn), 6)
})

test_that("lsm_l_gyrate_mn returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$level, "character")
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_gyrate_mn$value, "double")
})



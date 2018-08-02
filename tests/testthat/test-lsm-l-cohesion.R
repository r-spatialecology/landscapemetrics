context("landscape level cohesion metric")

fragstats_landscape_landscape_cohesion <- fragstats_landscape_landscape$COHESION
landscapemetrics_landscape_landscape_cohesion <- lsm_l_cohesion(landscape)

test_that("lsm_l_cohesion results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_cohesion %in%
                        round(landscapemetrics_landscape_landscape_cohesion$value, 4)))
})

test_that("lsm_l_cohesion is typestable", {
    expect_is(landscapemetrics_landscape_landscape_cohesion, "tbl_df")
    expect_is(lsm_l_cohesion(landscape_stack), "tbl_df")
    expect_is(lsm_l_cohesion(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_cohesion returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_cohesion), 6)
})

test_that("lsm_l_cohesion returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_cohesion$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_cohesion$level, "character")
    expect_type(landscapemetrics_landscape_landscape_cohesion$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_cohesion$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_cohesion$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_cohesion$value, "double")
})

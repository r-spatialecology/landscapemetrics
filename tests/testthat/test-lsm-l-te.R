context("landscape level ta metric")

fragstats_landscape_landscape_te <- fragstats_landscape_landscape$TE
landscapemetrics_landscape_landscape_te <- lsm_l_te(landscape)

test_that("lsm_l_te results are equal to fragstats", {
    expect_equal(landscapemetrics_landscape_landscape_te$value,
                 fragstats_landscape_landscape_te)
})

test_that("lsm_l_te is typestable", {
    expect_is(landscapemetrics_landscape_landscape_te, "tbl_df")
    expect_is(lsm_l_te(landscape_stack), "tbl_df")
    expect_is(lsm_l_te(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_te returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_te), 6)
})

test_that("lsm_l_te returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_te$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$level, "character")
    expect_type(landscapemetrics_landscape_landscape_te$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_te$value, "double")
})

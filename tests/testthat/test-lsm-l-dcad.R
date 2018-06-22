context("landscape level dcad metric")

fragstats_landscape_landscape_dcad <- fragstats_landscape_landscape$dcad
landscapemetrics_landscape_landscape_dcad <- lsm_l_dcad(landscape)

test_that("lsm_l_dcad results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_dcad %in%
                        round(landscapemetrics_landscape_landscape_dcad$value, 4)))
})

test_that("lsm_l_dcad is typestable", {
    expect_is(landscapemetrics_landscape_landscape_dcad, "tbl_df")
    expect_is(lsm_l_dcad(landscape_stack), "tbl_df")
    expect_is(lsm_l_dcad(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_dcad returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_dcad), 6)
})

test_that("lsm_l_dcad returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_dcad$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcad$level, "character")
    expect_type(landscapemetrics_landscape_landscape_dcad$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcad$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcad$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_dcad$value, "double")
})

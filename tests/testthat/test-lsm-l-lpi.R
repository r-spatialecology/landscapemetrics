context("landscape level lpi metric")

fragstats_landscape_landscape_lpi <- fragstats_landscape_landscape$LPI
landscapemetrics_landscape_landscape_lpi <- lsm_l_lpi(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_lpi %in%
                        round(landscapemetrics_landscape_landscape_lpi$value,4)))
})

test_that("lsm_c_lpi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_lpi, "tbl_df")
    expect_is(lsm_l_lpi(landscape_stack), "tbl_df")
    expect_is(lsm_l_lpi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_lpi), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_lpi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_lpi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_lpi$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_lpi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_lpi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_lpi$value, "double")
})



context("landscape level rpr metric")

fragstats_landscape_landscape_rpr <- fragstats_landscape_landscape$RPR
landscapemetrics_landscape_landscape_rpr <- lsm_l_rpr(landscape, 5)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_rpr %in%
                        round(landscapemetrics_landscape_landscape_rpr$value,4)))
})

test_that("lsm_c_rpr is typestable", {
    expect_is(landscapemetrics_landscape_landscape_rpr, "tbl_df")
    expect_is(lsm_l_rpr(landscape_stack), "tbl_df")
    expect_is(lsm_l_rpr(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_rpr), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_rpr$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_rpr$level, "character")
    expect_type(landscapemetrics_landscape_landscape_rpr$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_rpr$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_rpr$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_rpr$value, "double")
})



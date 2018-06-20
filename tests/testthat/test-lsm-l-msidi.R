context("landscape level msidi metric")

fragstats_landscape_landscape_msidi <- fragstats_landscape_landscape$MSIDI
landscapemetrics_landscape_landscape_msidi <- lsm_l_msidi(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_msidi %in%
                        round(landscapemetrics_landscape_landscape_msidi$value,4)))
})

test_that("lsm_c_msidi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_msidi, "tbl_df")
    expect_is(lsm_l_msidi(landscape_stack), "tbl_df")
    expect_is(lsm_l_msidi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_msidi), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_msidi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_msidi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_msidi$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_msidi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_msidi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_msidi$value, "double")
})



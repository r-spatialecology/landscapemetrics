context("landscape level sidi metric")

fragstats_landscape_landscape_sidi <- fragstats_landscape_landscape$SIDI
landscapemetrics_landscape_landscape_sidi <- lsm_l_sidi(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_sidi %in%
                        round(landscapemetrics_landscape_landscape_sidi$value,4)))
})

test_that("lsm_c_sidi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_sidi, "tbl_df")
    expect_is(lsm_l_sidi(landscape_stack), "tbl_df")
    expect_is(lsm_l_sidi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_sidi), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_sidi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_sidi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_sidi$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_sidi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_sidi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_sidi$value, "double")
})

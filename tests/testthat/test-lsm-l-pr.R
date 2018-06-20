context("landscape level pr metric")

fragstats_landscape_landscape_pr <- fragstats_landscape_landscape$PR
landscapemetrics_landscape_landscape_pr <- lsm_l_pr(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_pr %in%
                        round(landscapemetrics_landscape_landscape_pr$value,4)))
})

test_that("lsm_c_pr is typestable", {
    expect_is(landscapemetrics_landscape_landscape_pr, "tbl_df")
    expect_is(lsm_l_pr(landscape_stack), "tbl_df")
    expect_is(lsm_l_pr(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_pr), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_pr$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_pr$level, "character")
    expect_type(landscapemetrics_landscape_landscape_pr$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_pr$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_pr$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_pr$value, "double")
})



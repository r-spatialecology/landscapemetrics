context("landscape level np metric")

fragstats_landscape_landscape_np <- fragstats_landscape_landscape$NP
landscapemetrics_landscape_landscape_np <- lsm_l_np(landscape)

test_that("lsm_l_np results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_np %in%
                        round(landscapemetrics_landscape_landscape_np$value, 4)))
})

test_that("lsm_l_np is typestable", {
    expect_is(landscapemetrics_landscape_landscape_np, "tbl_df")
    expect_is(lsm_l_np(landscape_stack), "tbl_df")
    expect_is(lsm_l_np(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_np returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_np), 6)
})

test_that("lsm_l_np returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_np$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_np$level, "character")
    expect_type(landscapemetrics_landscape_landscape_np$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_np$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_np$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_np$value, "double")
})

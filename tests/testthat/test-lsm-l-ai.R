context("landscape level ai metric")

fragstats_landscape_landscape_ai <- fragstats_landscape_landscape$AI
landscapemetrics_landscape_landscape_ai <- lsm_l_ai(landscape)

test_that("lsm_l_circle_sd results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_ai %in%
                        round(landscapemetrics_landscape_landscape_ai$value, 4)))
})

test_that("lsm_l_ai is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ai, "tbl_df")
    expect_is(lsm_l_ai(landscape_stack), "tbl_df")
    expect_is(lsm_l_ai(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ai), 6)
})

test_that("lsm_l_ai returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ai$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ai$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ai$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ai$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ai$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ai$value, "double")
})


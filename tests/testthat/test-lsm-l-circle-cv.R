context("landscape level circle_cv metric")

fragstats_landscape_landscape_circle_cv <- fragstats_landscape_landscape$CIRCLE_CV
landscapemetrics_landscape_landscape_circle_cv <- lsm_l_circle_cv(landscape)

test_that("lsm_l_circle_cv results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_circle_cv %in%
                        round(landscapemetrics_landscape_landscape_circle_cv$value, 4)))
})

landscapemetrics_landscape_landscape_circle_cv <- lsm_l_circle_cv(landscape)

test_that("lsm_l_circle_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_circle_cv, "tbl_df")
    expect_is(lsm_l_circle_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_circle_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_circle_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_circle_cv), 6)
})

test_that("lsm_l_circle_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_circle_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_circle_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_circle_cv$value, "double")
})

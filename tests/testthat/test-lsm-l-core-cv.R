context("landscape level core_cv metric")

fragstats_landscape_landscape_core_cv <- fragstats_landscape_landscape$CORE_CV
landscapemetrics_landscape_landscape_core_cv <- lsm_l_core_cv(landscape)

test_that("lsm_l_core_cv results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_core_cv %in%
                        round(landscapemetrics_landscape_landscape_core_cv$value, 4)))
})

test_that("lsm_l_core_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_core_cv, "tbl_df")
    expect_is(lsm_l_core_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_core_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_core_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_core_cv), 6)
})

test_that("lsm_l_core_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_core_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_core_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_core_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_core_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_core_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_core_cv$value, "double")
})



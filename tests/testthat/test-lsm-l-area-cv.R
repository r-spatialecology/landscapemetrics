context("landscape level area_cv metric")

fragstats_landscape_landscape_area_cv <- fragstats_landscape_landscape$AREA_CV
landscapemetrics_landscape_landscape_area_cv <- lsm_l_area_cv(landscape)

test_that("lsm_l_area_cv results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_area_cv %in%
                        round(landscapemetrics_landscape_landscape_area_cv$value, 4)))
})

test_that("lsm_l_area_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_area_cv, "tbl_df")
    expect_is(lsm_l_area_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_area_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_area_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_area_cv), 6)
})

test_that("lsm_l_area_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_area_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_area_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_area_cv$value, "double")
})

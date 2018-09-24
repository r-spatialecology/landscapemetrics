context("landscape level lsm_l_frac_cv metric")

fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
    summarise(value = cv(FRAC))

landscapemetrics_landscape_landscape_value <- lsm_l_frac_cv(landscape)

test_that("lsm_l_frac_cv results are equal to fragstats", {
    expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
                    round(landscapemetrics_landscape_landscape_value$value, 4))
})

test_that("lsm_l_frac_cv is typestable", {
    expect_is(lsm_l_frac_cv(landscape), "tbl_df")
    expect_is(lsm_l_frac_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_frac_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_frac_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_frac_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})

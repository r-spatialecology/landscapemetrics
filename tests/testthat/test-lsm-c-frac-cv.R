context("class level frac_cv metric")

fragstats_class_landscape_frac_cv <- fragstats_class_landscape$frac_cv
landscapemetrics_class_landscape_frac_cv <- lsm_c_frac_cv(landscape)

test_that("lsm_c_frac_cv results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_frac_cv %in%
                        round(landscapemetrics_class_landscape_frac_cv$value, 4)))
})

test_that("lsm_c_frac_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_frac_cv, "tbl_df")
    expect_is(lsm_c_frac_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_frac_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_frac_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_frac_cv), 6)
})

test_that("lsm_c_frac_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_frac_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_frac_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_frac_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_frac_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_frac_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_frac_cv$value, "double")
})



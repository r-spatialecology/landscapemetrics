context("class level enn_cv metric")

fragstats_class_landscape_enn_cv <- fragstats_class_landscape$ENN_CV
landscapemetrics_class_landscape_enn_cv <- lsm_c_enn_cv(landscape)

test_that("lsm_c_enn_cv results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_enn_cv %in%
                        round(landscapemetrics_class_landscape_enn_cv$value, 4)))
})

test_that("lsm_c_enn_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_enn_cv, "tbl_df")
    expect_is(lsm_c_enn_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_enn_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_enn_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_enn_cv), 6)
})

test_that("lsm_c_enn_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_enn_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_enn_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_enn_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_enn_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_enn_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_enn_cv$value, "double")
})

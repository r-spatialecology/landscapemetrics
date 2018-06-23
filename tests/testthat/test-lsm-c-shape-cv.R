context("class level shape_cv metric")

fragstats_class_landscape_shape_cv <- fragstats_class_landscape$SHAPE_CV
landscapemetrics_class_landscape_shape_cv <- lsm_c_shape_cv(landscape)

test_that("lsm_c_shape_cv results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_shape_cv %in%
                        round(landscapemetrics_class_landscape_shape_cv$value, 4)))
})

test_that("lsm_c_shape_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_shape_cv, "tbl_df")
    expect_is(lsm_c_shape_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_shape_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_shape_cv returns the desirshape_cv number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_shape_cv), 6)
})

test_that("lsm_c_shape_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_shape_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_shape_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_shape_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_shape_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_shape_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_shape_cv$value, "double")
})



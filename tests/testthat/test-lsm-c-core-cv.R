context("class level core_cv metric")

landscapemetrics_class_landscape_core_cv <- lsm_c_core_cv(landscape)

test_that("lsm_c_core_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_core_cv, "tbl_df")
    expect_is(lsm_c_core_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_core_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_core_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_core_cv), 6)
})

test_that("lsm_p_core_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_core_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_core_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_core_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_core_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_core_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_core_cv$value, "double")
})



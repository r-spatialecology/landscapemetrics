context("class level para_cv metric")

landscapemetrics_class_landscape_para_cv <- lsm_c_para_cv(landscape)

test_that("lsm_c_para_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_para_cv, "tbl_df")
    expect_is(lsm_c_para_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_para_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_para_cv returns the desirpara_cv number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_para_cv), 6)
})

test_that("lsm_p_para_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_para_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_para_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_para_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_para_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_para_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_para_cv$value, "double")
})



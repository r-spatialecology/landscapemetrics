context("landscape level dcore_cv metric")

landscapemetrics_landscape_landscape_dcore_cv <- lsm_l_dcore_cv(landscape)

test_that("lsm_c_dcore_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_dcore_cv, "tbl_df")
    expect_is(lsm_l_dcore_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_dcore_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_dcore_cv), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_dcore_cv$value, "double")
})



context("landscape level ncore_cv metric")

landscapemetrics_landscape_landscape_ncore_cv <- lsm_l_ncore_cv(landscape)

test_that("lsm_c_ncore_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ncore_cv, "tbl_df")
    expect_is(lsm_l_ncore_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_ncore_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ncore_cv), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ncore_cv$value, "double")
})



context("landscape level area_cv metric")

landscapemetrics_landscape_landscape_area_cv <- lsm_l_area_cv(landscape)

test_that("lsm_c_area_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_area_cv, "tbl_df")
    expect_is(lsm_l_area_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_area_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_area_cv), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_area_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_area_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_area_cv$value, "double")
})



context("class level area cv metric")

landscapemetrics_class_landscape_area <- lsm_c_area_cv(landscape)

test_that("lsm_c_area_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_area, "tbl_df")
    expect_is(lsm_c_area_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_area_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_area), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_area$layer, "integer")
    expect_type(landscapemetrics_class_landscape_area$level, "character")
    expect_type(landscapemetrics_class_landscape_area$class, "integer")
    expect_type(landscapemetrics_class_landscape_area$id, "integer")
    expect_type(landscapemetrics_class_landscape_area$metric, "character")
    expect_type(landscapemetrics_class_landscape_area$value, "double")
})



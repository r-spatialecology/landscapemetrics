context("class level ncore metric")

landscapemetrics_class_landscape_ncore <- lsm_c_ncore(landscape)

test_that("lsm_c_ncore is typestable", {
    expect_is(landscapemetrics_class_landscape_ncore, "tbl_df")
    expect_is(lsm_c_ncore(landscape_stack), "tbl_df")
    expect_is(lsm_c_ncore(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_ncore returns the desirncore number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ncore), 6)
})

test_that("lsm_p_ncore returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ncore$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ncore$level, "character")
    expect_type(landscapemetrics_class_landscape_ncore$class, "integer")
    expect_type(landscapemetrics_class_landscape_ncore$id, "integer")
    expect_type(landscapemetrics_class_landscape_ncore$metric, "character")
    expect_type(landscapemetrics_class_landscape_ncore$value, "double")
})



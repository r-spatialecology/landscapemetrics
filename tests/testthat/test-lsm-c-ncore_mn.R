context("class level ncore_mn metric")

landscapemetrics_class_landscape_ncore_mn <- lsm_c_ncore_mn(landscape)

test_that("lsm_c_ncore_mn is typestable", {
    expect_is(landscapemetrics_class_landscape_ncore_mn, "tbl_df")
    expect_is(lsm_c_ncore_mn(landscape_stack), "tbl_df")
    expect_is(lsm_c_ncore_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_ncore_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ncore_mn), 6)
})

test_that("lsm_p_ncore_mn returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ncore_mn$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ncore_mn$level, "character")
    expect_type(landscapemetrics_class_landscape_ncore_mn$class, "integer")
    expect_type(landscapemetrics_class_landscape_ncore_mn$id, "integer")
    expect_type(landscapemetrics_class_landscape_ncore_mn$metric, "character")
    expect_type(landscapemetrics_class_landscape_ncore_mn$value, "double")
})



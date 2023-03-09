context("landscape level lsm_l_rpr metric")

landscapemetrics_landscape_landscape_value <- lsm_l_rpr(landscape, classes_max = 5)

test_that("lsm_l_rpr is typestable", {
    expect_is(lsm_l_rpr(landscape, classes_max = 5), "tbl_df")
    expect_is(lsm_l_rpr(landscape_stack, classes_max = 5), "tbl_df")
    expect_is(lsm_l_rpr(landscape_list, classes_max = 5), "tbl_df")
})

test_that("lsm_l_rpr returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_rpr returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})


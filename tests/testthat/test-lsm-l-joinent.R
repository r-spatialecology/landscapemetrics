context("landscape level joinent metric")

landscapemetrics_landscape_landscape_joinent <- lsm_l_joinent(landscape)

test_that("lsm_l_joinent is typestable", {
    expect_is(landscapemetrics_landscape_landscape_joinent, "tbl_df")
    expect_is(lsm_l_joinent(landscape_stack), "tbl_df")
    expect_is(lsm_l_joinent(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_joinent returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_joinent), 6)
})

test_that("lsm_l_joinent returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_joinent$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_joinent$level, "character")
    expect_type(landscapemetrics_landscape_landscape_joinent$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_joinent$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_joinent$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_joinent$value, "double")
})

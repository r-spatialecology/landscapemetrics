context("landscape level mutinf metric")

landscapemetrics_landscape_landscape_mutinf <- lsm_l_mutinf(landscape)

test_that("lsm_l_mutinf is typestable", {
    expect_is(landscapemetrics_landscape_landscape_mutinf, "tbl_df")
    expect_is(lsm_l_mutinf(landscape_stack), "tbl_df")
    expect_is(lsm_l_mutinf(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_mutinf returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_mutinf), 6)
})

test_that("lsm_l_mutinf returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_mutinf$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_mutinf$level, "character")
    expect_type(landscapemetrics_landscape_landscape_mutinf$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_mutinf$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_mutinf$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_mutinf$value, "double")
})

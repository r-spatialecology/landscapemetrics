context("landscape level condent metric")

landscapemetrics_landscape_landscape_condent <- lsm_l_condent(landscape)

test_that("lsm_l_condent is typestable", {
    expect_is(landscapemetrics_landscape_landscape_condent, "tbl_df")
    expect_is(lsm_l_condent(landscape_stack), "tbl_df")
    expect_is(lsm_l_condent(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_condent returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_condent), 6)
})

test_that("lsm_l_condent returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_condent$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_condent$level, "character")
    expect_type(landscapemetrics_landscape_landscape_condent$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_condent$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_condent$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_condent$value, "double")
})

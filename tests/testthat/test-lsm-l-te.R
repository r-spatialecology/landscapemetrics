context("landscape level ta metric")

landscapemetrics_landscape_landscape_te <- lsm_l_te(landscape)

test_that("lsm_c_te is typestable", {
    expect_is(landscapemetrics_landscape_landscape_te, "tbl_df")
    expect_is(lsm_l_te(landscape_stack), "tbl_df")
    expect_is(lsm_l_te(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_te), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_te$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$level, "character")
    expect_type(landscapemetrics_landscape_landscape_te$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_te$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_te$value, "double")
})

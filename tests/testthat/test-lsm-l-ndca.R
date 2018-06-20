context("landscape level ndca metric")

landscapemetrics_landscape_landscape_ndca <- lsm_l_ndca(landscape)

test_that("lsm_c_ndca is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ndca, "tbl_df")
    expect_is(lsm_l_ndca(landscape_stack), "tbl_df")
    expect_is(lsm_l_ndca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ndca), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ndca$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ndca$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ndca$value, "double")
})



context("landscape level area_sd metric")

landscapemetrics_landscape_landscape_area_sd <- lsm_l_area_sd(landscape)

test_that("lsm_c_area_sd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_area_sd, "tbl_df")
    expect_is(lsm_l_area_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_area_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_area_sd), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_area_sd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_sd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_area_sd$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_sd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_area_sd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_area_sd$value, "double")
})



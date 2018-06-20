context("landscape level circle_sd metric")

landscapemetrics_landscape_landscape_circle_sd <- lsm_l_circle_sd(landscape)

test_that("lsm_c_circle_sd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_circle_sd, "tbl_df")
    expect_is(lsm_l_circle_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_circle_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_circle_sd), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_circle_sd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_sd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_circle_sd$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_sd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_circle_sd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_circle_sd$value, "double")
})



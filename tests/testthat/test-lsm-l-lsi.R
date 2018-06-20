context("landscape level lsi metric")

landscapemetrics_landscape_landscape_lsi <- lsm_l_lsi(landscape)

test_that("lsm_c_lsi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_lsi, "tbl_df")
    expect_is(lsm_l_lsi(landscape_stack), "tbl_df")
    expect_is(lsm_l_lsi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_lsi), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_lsi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_lsi$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_lsi$value, "double")
})



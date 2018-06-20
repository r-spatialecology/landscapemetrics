context("landscape level ncore metric")

landscapemetrics_landscape_landscape_ncore <- lsm_l_ncore(landscape)

test_that("lsm_c_ncore is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ncore, "tbl_df")
    expect_is(lsm_l_ncore(landscape_stack), "tbl_df")
    expect_is(lsm_l_ncore(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ncore), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ncore$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ncore$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ncore$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ncore$value, "double")
})



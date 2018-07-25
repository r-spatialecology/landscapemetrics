context("landscape level ent metric")

landscapemetrics_landscape_landscape_ent <- lsm_l_ent(landscape)

test_that("lsm_l_ent is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ent, "tbl_df")
    expect_is(lsm_l_ent(landscape_stack), "tbl_df")
    expect_is(lsm_l_ent(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ent returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ent), 6)
})

test_that("lsm_l_ent returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ent$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ent$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ent$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ent$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ent$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ent$value, "double")
})

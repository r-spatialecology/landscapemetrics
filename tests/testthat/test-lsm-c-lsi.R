context("class level lsi metric")

landscapemetrics_class_landscape_lsi <- lsm_c_lsi(landscape)

test_that("lsm_c_lsi is typestable", {
    expect_is(landscapemetrics_class_landscape_lsi, "tbl_df")
    expect_is(lsm_c_lsi(landscape_stack), "tbl_df")
    expect_is(lsm_c_lsi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_lsi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_lsi), 6)
})

test_that("lsm_p_lsi returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_lsi$layer, "integer")
    expect_type(landscapemetrics_class_landscape_lsi$level, "character")
    expect_type(landscapemetrics_class_landscape_lsi$class, "integer")
    expect_type(landscapemetrics_class_landscape_lsi$id, "integer")
    expect_type(landscapemetrics_class_landscape_lsi$metric, "character")
    expect_type(landscapemetrics_class_landscape_lsi$value, "double")
})



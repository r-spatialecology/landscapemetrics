context("class level pafrac metric")

landscapemetrics_class_landscape_pafrac <- lsm_c_pafrac(landscape)

test_that("lsm_c_pafrac is typestable", {
    expect_is(landscapemetrics_class_landscape_pafrac, "tbl_df")
    expect_is(lsm_c_pafrac(landscape_stack), "tbl_df")
    expect_is(lsm_c_pafrac(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_pafrac returns the desirpafrac number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_pafrac), 6)
})

test_that("lsm_p_pafrac returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_pafrac$layer, "integer")
    expect_type(landscapemetrics_class_landscape_pafrac$level, "character")
    expect_type(landscapemetrics_class_landscape_pafrac$class, "integer")
    expect_type(landscapemetrics_class_landscape_pafrac$id, "integer")
    expect_type(landscapemetrics_class_landscape_pafrac$metric, "character")
    expect_type(landscapemetrics_class_landscape_pafrac$value, "double")
})



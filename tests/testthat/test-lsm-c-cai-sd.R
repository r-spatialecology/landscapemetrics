context("class level cai_sd metric")

landscapemetrics_class_landscape_cai_sd <- lsm_c_cai_sd(landscape)

test_that("lsm_c_cai_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_cai_sd, "tbl_df")
    expect_is(lsm_c_cai_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_cai_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_cai_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_cai_sd), 6)
})

test_that("lsm_p_cai_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_cai_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_cai_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_cai_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_cai_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_cai_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_cai_sd$value, "double")
})



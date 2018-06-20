context("class level para_sd metric")

landscapemetrics_class_landscape_para_sd <- lsm_c_para_sd(landscape)

test_that("lsm_c_para_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_para_sd, "tbl_df")
    expect_is(lsm_c_para_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_para_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_para_sd returns the desirpara_sd number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_para_sd), 6)
})

test_that("lsm_p_para_sd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_para_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_para_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_para_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_para_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_para_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_para_sd$value, "double")
})



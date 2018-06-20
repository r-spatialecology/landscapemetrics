context("class level enn_mn metric")

landscapemetrics_class_landscape_enn_mn <- lsm_c_enn_mn(landscape)

test_that("lsm_c_enn_mn is typestable", {
    expect_is(landscapemetrics_class_landscape_enn_mn, "tbl_df")
    expect_is(lsm_c_enn_mn(landscape_stack), "tbl_df")
    expect_is(lsm_c_enn_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_enn_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_enn_mn), 6)
})

test_that("lsm_p_enn_mn returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_enn_mn$layer, "integer")
    expect_type(landscapemetrics_class_landscape_enn_mn$level, "character")
    expect_type(landscapemetrics_class_landscape_enn_mn$class, "integer")
    expect_type(landscapemetrics_class_landscape_enn_mn$id, "integer")
    expect_type(landscapemetrics_class_landscape_enn_mn$metric, "character")
    expect_type(landscapemetrics_class_landscape_enn_mn$value, "double")
})



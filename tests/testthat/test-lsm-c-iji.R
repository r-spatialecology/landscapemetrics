context("class level lsm_c_iji metric")

landscapemetrics_class_landscape_value <- lsm_c_iji(landscape)

test_that("lsm_c_iji is typestable", {

    expect_is(lsm_c_iji(landscape), "tbl_df")
    expect_is(lsm_c_iji(landscape_stack), "tbl_df")
    expect_is(lsm_c_iji(landscape_brick), "tbl_df")
    expect_is(lsm_c_iji(landscape_list), "tbl_df")
})

test_that("lsm_c_iji returns the desired number of columns", {

    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_iji returns in every column the correct type", {

    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_iji returns warning for less than 3 classes", {

    expect_warning(lsm_c_iji(landscape_simple),
                   grep = "Number of classes must be >= 3, IJI = NA.",
                   fixed = TRUE)
})


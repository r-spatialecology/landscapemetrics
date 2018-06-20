context("class level te metric")

fragstats_class_landscape_te <- fragstats_class_landscape$TE
landscapemetrics_class_landscape_te <- lsm_c_te(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_te %in%
                        round(landscapemetrics_class_landscape_te$value,4)))
})

test_that("lsm_c_te is typestable", {
    expect_is(landscapemetrics_class_landscape_te, "tbl_df")
    expect_is(lsm_c_te(landscape_stack), "tbl_df")
    expect_is(lsm_c_te(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_te returns the desirte number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_te), 6)
})

test_that("lsm_p_te returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_te$layer, "integer")
    expect_type(landscapemetrics_class_landscape_te$level, "character")
    expect_type(landscapemetrics_class_landscape_te$class, "integer")
    expect_type(landscapemetrics_class_landscape_te$id, "integer")
    expect_type(landscapemetrics_class_landscape_te$metric, "character")
    expect_type(landscapemetrics_class_landscape_te$value, "double")
})



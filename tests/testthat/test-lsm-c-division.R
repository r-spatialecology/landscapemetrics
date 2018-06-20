context("class level division metric")

fragstats_class_landscape_division <- fragstats_class_landscape$DIVISION
landscapemetrics_class_landscape_division <- lsm_c_division(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_division %in%
                        round(landscapemetrics_class_landscape_division$value,4)))
})

test_that("lsm_c_division is typestable", {
    expect_is(landscapemetrics_class_landscape_division, "tbl_df")
    expect_is(lsm_c_division(landscape_stack), "tbl_df")
    expect_is(lsm_c_division(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_division returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_division), 6)
})

test_that("lsm_p_division returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_division$layer, "integer")
    expect_type(landscapemetrics_class_landscape_division$level, "character")
    expect_type(landscapemetrics_class_landscape_division$class, "integer")
    expect_type(landscapemetrics_class_landscape_division$id, "integer")
    expect_type(landscapemetrics_class_landscape_division$metric, "character")
    expect_type(landscapemetrics_class_landscape_division$value, "double")
})



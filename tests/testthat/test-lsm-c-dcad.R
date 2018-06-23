context("class level dcad metric")

fragstats_class_landscape_dcad <- fragstats_class_landscape$DCAD
landscapemetrics_class_landscape_dcad <- lsm_c_dcad(landscape)

test_that("lsm_c_dcad results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_dcad %in%
                        round(landscapemetrics_class_landscape_dcad$value, 4)))
})

test_that("lsm_c_dcad is typestable", {
    expect_is(landscapemetrics_class_landscape_dcad, "tbl_df")
    expect_is(lsm_c_dcad(landscape_stack), "tbl_df")
    expect_is(lsm_c_dcad(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_dcad returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_dcad), 6)
})

test_that("lsm_c_dcad returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_dcad$layer, "integer")
    expect_type(landscapemetrics_class_landscape_dcad$level, "character")
    expect_type(landscapemetrics_class_landscape_dcad$class, "integer")
    expect_type(landscapemetrics_class_landscape_dcad$id, "integer")
    expect_type(landscapemetrics_class_landscape_dcad$metric, "character")
    expect_type(landscapemetrics_class_landscape_dcad$value, "double")
})



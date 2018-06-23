context("class level ed metric")

fragstats_class_landscape_ed <- fragstats_class_landscape$ED
landscapemetrics_class_landscape_ed <- lsm_c_ed(landscape)

test_that("lsm_c_ed results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_ed %in%
                        round(landscapemetrics_class_landscape_ed$value, 4)))
})

test_that("lsm_c_ed is typestable", {
    expect_is(landscapemetrics_class_landscape_ed, "tbl_df")
    expect_is(lsm_c_ed(landscape_stack), "tbl_df")
    expect_is(lsm_c_ed(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_ed returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ed), 6)
})

test_that("lsm_c_ed returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ed$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ed$level, "character")
    expect_type(landscapemetrics_class_landscape_ed$class, "integer")
    expect_type(landscapemetrics_class_landscape_ed$id, "integer")
    expect_type(landscapemetrics_class_landscape_ed$metric, "character")
    expect_type(landscapemetrics_class_landscape_ed$value, "double")
})



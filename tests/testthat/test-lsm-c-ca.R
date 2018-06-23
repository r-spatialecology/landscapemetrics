context("class level ca metric")

fragstats_class_landscape_ca <- fragstats_class_landscape$CA
landscapemetrics_class_landscape_ca <- lsm_c_ca(landscape)

test_that("lsm_c_ca results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_ca %in%
                        round(landscapemetrics_class_landscape_ca$value, 4)))
})

test_that("lsm_c_ca is typestable", {
    expect_is(landscapemetrics_class_landscape_ca, "tbl_df")
    expect_is(lsm_c_ca(landscape_stack), "tbl_df")
    expect_is(lsm_c_ca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_ca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ca), 6)
})

test_that("lsm_c_ca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ca$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ca$level, "character")
    expect_type(landscapemetrics_class_landscape_ca$class, "integer")
    expect_type(landscapemetrics_class_landscape_ca$id, "integer")
    expect_type(landscapemetrics_class_landscape_ca$metric, "character")
    expect_type(landscapemetrics_class_landscape_ca$value, "double")
})



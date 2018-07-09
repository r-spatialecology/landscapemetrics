context("class level clumpy metric")

fragstats_class_landscape_clumpy <- fragstats_class_landscape$CLUMPY
landscapemetrics_class_landscape_clumpy <- lsm_c_clumpy(landscape)

test_that("lsm_c_clumpy results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_clumpy %in%
                        round(landscapemetrics_class_landscape_clumpy$value, 4)))
})

test_that("lsm_c_clumpy is typestable", {
    expect_is(landscapemetrics_class_landscape_clumpy, "tbl_df")
    expect_is(lsm_c_clumpy(landscape_stack), "tbl_df")
    expect_is(lsm_c_clumpy(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_clumpy returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_clumpy), 6)
})

test_that("lsm_c_clumpy returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_clumpy$layer, "integer")
    expect_type(landscapemetrics_class_landscape_clumpy$level, "character")
    expect_type(landscapemetrics_class_landscape_clumpy$class, "integer")
    expect_type(landscapemetrics_class_landscape_clumpy$id, "integer")
    expect_type(landscapemetrics_class_landscape_clumpy$metric, "character")
    expect_type(landscapemetrics_class_landscape_clumpy$value, "double")
})



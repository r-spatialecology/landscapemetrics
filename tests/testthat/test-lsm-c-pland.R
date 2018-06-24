context("class level pland metric")

fragstats_class_landscape_pland <- fragstats_class_landscape$PLAND
landscapemetrics_class_landscape_pland <- lsm_c_pland(landscape)

test_that("lsm_c_pland results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_pland %in%
                        round(landscapemetrics_class_landscape_pland$value, 4)))
})

test_that("lsm_c_pland is typestable", {
    expect_is(landscapemetrics_class_landscape_pland, "tbl_df")
    expect_is(lsm_c_pland(landscape_stack), "tbl_df")
    expect_is(lsm_c_pland(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_pland returns the desirpland number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_pland), 6)
})

test_that("lsm_c_pland returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_pland$layer, "integer")
    expect_type(landscapemetrics_class_landscape_pland$level, "character")
    expect_type(landscapemetrics_class_landscape_pland$class, "integer")
    expect_type(landscapemetrics_class_landscape_pland$id, "integer")
    expect_type(landscapemetrics_class_landscape_pland$metric, "character")
    expect_type(landscapemetrics_class_landscape_pland$value, "double")
})



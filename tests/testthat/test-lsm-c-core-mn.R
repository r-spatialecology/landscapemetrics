context("class level core_mn metric")

fragstats_class_landscape_core_mn <- fragstats_class_landscape$CORE_MN
landscapemetrics_class_landscape_core_mn <- lsm_c_core_mn(landscape)

test_that("lsm_c_core_mn results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_core_mn %in%
                        round(landscapemetrics_class_landscape_core_mn$value, 4)))
})

test_that("lsm_c_core_mn is typestable", {
    expect_is(landscapemetrics_class_landscape_core_mn, "tbl_df")
    expect_is(lsm_c_core_mn(landscape_stack), "tbl_df")
    expect_is(lsm_c_core_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_core_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_core_mn), 6)
})

test_that("lsm_c_core_mn returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_core_mn$layer, "integer")
    expect_type(landscapemetrics_class_landscape_core_mn$level, "character")
    expect_type(landscapemetrics_class_landscape_core_mn$class, "integer")
    expect_type(landscapemetrics_class_landscape_core_mn$id, "integer")
    expect_type(landscapemetrics_class_landscape_core_mn$metric, "character")
    expect_type(landscapemetrics_class_landscape_core_mn$value, "double")
})



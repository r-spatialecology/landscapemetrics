context("class level cpland metric")

# fragstats_class_landscape_cpland <- fragstats_class_landscape$CPLAND
landscapemetrics_class_landscape_cpland <- lsm_c_cpland(landscape)
#
# test_that("lsm_c_cpland results are equal to fragstats", {
#     expect_true(all(fragstats_class_landscape_cpland %in%
#                         round(landscapemetrics_class_landscape_cpland$value, 4)))
# })

test_that("lsm_c_cpland is typestable", {
    expect_is(landscapemetrics_class_landscape_cpland, "tbl_df")
    expect_is(lsm_c_cpland(landscape_stack), "tbl_df")
    expect_is(lsm_c_cpland(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_cpland returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_cpland), 6)
})

test_that("lsm_c_cpland returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_cpland$layer, "integer")
    expect_type(landscapemetrics_class_landscape_cpland$level, "character")
    expect_type(landscapemetrics_class_landscape_cpland$class, "integer")
    expect_type(landscapemetrics_class_landscape_cpland$id, "integer")
    expect_type(landscapemetrics_class_landscape_cpland$metric, "character")
    expect_type(landscapemetrics_class_landscape_cpland$value, "double")
})



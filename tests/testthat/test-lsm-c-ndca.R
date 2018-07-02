context("class level ndca metric")

# fragstats_class_landscape_ndca <- fragstats_class_landscape$NDCA
landscapemetrics_class_landscape_ndca <- lsm_c_ndca(landscape)
#
# test_that("lsm_c_ndca results are equal to fragstats", {
#     expect_true(all(fragstats_class_landscape_ndca %in%
#                         round(landscapemetrics_class_landscape_ndca$value, 4)))
# })

test_that("lsm_c_ndca is typestable", {
    expect_is(landscapemetrics_class_landscape_ndca, "tbl_df")
    expect_is(lsm_c_ndca(landscape_stack), "tbl_df")
    expect_is(lsm_c_ndca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_ndca returns the desirncore number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ndca), 6)
})

test_that("lsm_c_ndca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ndca$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ndca$level, "character")
    expect_type(landscapemetrics_class_landscape_ndca$class, "integer")
    expect_type(landscapemetrics_class_landscape_ndca$id, "integer")
    expect_type(landscapemetrics_class_landscape_ndca$metric, "character")
    expect_type(landscapemetrics_class_landscape_ndca$value, "double")
})

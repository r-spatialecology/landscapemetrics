context("landscape level ndca metric")

# fragstats_landscape_landscape_ndca <- fragstats_landscape_landscape$NDCA
landscapemetrics_landscape_landscape_ndca <- lsm_l_ndca(landscape)
#
# test_that("lsm_l_ndca results are equal to fragstats", {
#     expect_true(all(fragstats_landscape_landscape_ndca %in%
#                         round(landscapemetrics_landscape_landscape_ndca$value, 4)))
# })

test_that("lsm_l_ndca is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ndca, "tbl_df")
    expect_is(lsm_l_ndca(landscape_stack), "tbl_df")
    expect_is(lsm_l_ndca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ndca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ndca), 6)
})

test_that("lsm_l_ndca returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ndca$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ndca$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ndca$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ndca$value, "double")
})


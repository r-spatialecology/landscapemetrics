context("landscape level dcore_sd metric")

# fragstats_landscape_landscape_dcore_sd <- fragstats_landscape_landscape$DCORE_SD
landscapemetrics_landscape_landscape_dcore_sd <- lsm_l_dcore_sd(landscape)
#
# test_that("lsm_l_dcore_sd results are equal to fragstats", {
#     expect_true(all(fragstats_landscape_landscape_dcore_sd %in%
#                         round(landscapemetrics_landscape_landscape_dcore_sd$value, 4)))
# })

test_that("lsm_l_dcore_sd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_dcore_sd, "tbl_df")
    expect_is(lsm_l_dcore_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_dcore_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_dcore_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_dcore_sd), 6)
})

test_that("lsm_l_dcore_sd returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_dcore_sd$value, "double")
})



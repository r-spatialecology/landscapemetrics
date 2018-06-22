context("landscape level shdi metric")

fragstats_landscape_landscape_shdi <- fragstats_landscape_landscape$SHDI
landscapemetrics_landscape_landscape_shdi <- lsm_l_shdi(landscape)

test_that("lsm_l_shdi results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_shdi %in%
                        round(landscapemetrics_landscape_landscape_shdi$value, 4)))
})

test_that("lsm_l_shdi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_shdi, "tbl_df")
    expect_is(lsm_l_shdi(landscape_stack), "tbl_df")
    expect_is(lsm_l_shdi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_shdi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_shdi), 6)
})

test_that("lsm_l_shdi returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_shdi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_shdi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_shdi$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_shdi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_shdi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_shdi$value, "double")
})

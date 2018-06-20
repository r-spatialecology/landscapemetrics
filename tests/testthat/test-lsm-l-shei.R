context("landscape level shei metric")

fragstats_landscape_landscape_shei <- fragstats_landscape_landscape$SHEI
landscapemetrics_landscape_landscape_shei <- lsm_l_shei(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_shei %in%
                        round(landscapemetrics_landscape_landscape_shei$value,4)))
})

test_that("lsm_c_shei is typestable", {
    expect_is(landscapemetrics_landscape_landscape_shei, "tbl_df")
    expect_is(lsm_l_shei(landscape_stack), "tbl_df")
    expect_is(lsm_l_shei(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_shei), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_shei$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_shei$level, "character")
    expect_type(landscapemetrics_landscape_landscape_shei$landscape, "integer")
    expect_type(landscapemetrics_landscape_landscape_shei$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_shei$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_shei$value, "double")
})



context("landscape level split metric")

fragstats_landscape_landscape_split <- fragstats_landscape_landscape$SPLIT
landscapemetrics_landscape_landscape_split <- lsm_l_split(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_split %in%
                        round(landscapemetrics_landscape_landscape_split$value,4)))
})

test_that("lsm_c_split is typestable", {
    expect_is(landscapemetrics_landscape_landscape_split, "tbl_df")
    expect_is(lsm_l_split(landscape_stack), "tbl_df")
    expect_is(lsm_l_split(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_split), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_split$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_split$level, "character")
    expect_type(landscapemetrics_landscape_landscape_split$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_split$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_split$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_split$value, "double")
})

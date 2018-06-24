context("landscape level ed metric")

fragstats_landscape_landscape_ed <- fragstats_landscape_landscape$ED
landscapemetrics_landscape_landscape_ed <- lsm_l_ed(landscape)

test_that("lsm_l_ed results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_ed %in%
                        round(landscapemetrics_landscape_landscape_ed$value, 4)))
})

test_that("lsm_l_ed is typestable", {
    expect_is(landscapemetrics_landscape_landscape_ed, "tbl_df")
    expect_is(lsm_l_ed(landscape_stack), "tbl_df")
    expect_is(lsm_l_ed(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_ed returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_ed), 6)
})

test_that("lsm_l_ed returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_ed$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_ed$level, "character")
    expect_type(landscapemetrics_landscape_landscape_ed$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_ed$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_ed$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_ed$value, "double")
})



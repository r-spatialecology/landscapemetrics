context("landscape level pafrac metric")

fragstats_landscape_landscape_pafrac <- fragstats_landscape_landscape$PAFRAC
landscapemetrics_landscape_landscape_pafrac <- lsm_l_pafrac(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_pafrac %in%
                        round(landscapemetrics_landscape_landscape_pafrac$value, 4)))
})

test_that("lsm_c_pafrac is typestable", {
    expect_is(landscapemetrics_landscape_landscape_pafrac, "tbl_df")
    expect_is(lsm_l_pafrac(landscape_stack), "tbl_df")
    expect_is(lsm_l_pafrac(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_area returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_pafrac), 6)
})

test_that("lsm_p_area returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_pafrac$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_pafrac$level, "character")
    expect_type(landscapemetrics_landscape_landscape_pafrac$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_pafrac$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_pafrac$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_pafrac$value, "double")
})



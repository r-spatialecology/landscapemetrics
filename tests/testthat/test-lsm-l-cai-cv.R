context("landscape level cai_cv metric")

fragstats_landscape_landscape_cai_cv <- fragstats_landscape_landscape$CAI_CV
landscapemetrics_landscape_landscape_cai_cv <- lsm_l_cai_cv(landscape)

test_that("lsm_l_cai_cv results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_cai_cv %in%
                        round(landscapemetrics_landscape_landscape_cai_cv$value, 4)))
})

test_that("lsm_l_cai_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_cai_cv, "tbl_df")
    expect_is(lsm_l_cai_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_cai_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_cai_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_cai_cv), 6)
})

test_that("lsm_l_cai_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_cai_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_cai_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_cai_cv$value, "double")
})

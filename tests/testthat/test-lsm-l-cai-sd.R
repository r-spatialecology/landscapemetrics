context("landscape level cai_sd metric")

fragstats_landscape_landscape_cai_sd <- fragstats_landscape_landscape$CAI_SD
landscapemetrics_landscape_landscape_cai_sd <- lsm_l_cai_sd(landscape)

test_that("lsm_l_cai_sd results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_cai_sd %in%
                        round(landscapemetrics_landscape_landscape_cai_sd$value, 4)))
})

test_that("lsm_l_cai_sd is typestable", {
    expect_is(landscapemetrics_landscape_landscape_cai_sd, "tbl_df")
    expect_is(lsm_l_cai_sd(landscape_stack), "tbl_df")
    expect_is(lsm_l_cai_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_cai_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_cai_sd), 6)
})

test_that("lsm_l_cai_sd returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_cai_sd$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_sd$level, "character")
    expect_type(landscapemetrics_landscape_landscape_cai_sd$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_sd$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_cai_sd$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_cai_sd$value, "double")
})

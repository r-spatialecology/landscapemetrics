context("landscape level lsm_l_enn_mn metric")

fragstats_landscape_landscape_area_value <- fragstats_patch_landscape %>%
    dplyr::summarize(value = mean(ENN))

landscapemetrics_landscape_landscape_value <- lsm_l_enn_mn(landscape)

test_that("lsm_l_enn_mn results are equal to fragstats", {
    expect_true(round(fragstats_landscape_landscape_area_value$value, 4) ==
                    round(landscapemetrics_landscape_landscape_value$value, 4))
})

test_that("lsm_l_enn_mn is typestable", {
    expect_is(lsm_l_enn_mn(landscape), "tbl_df")
    expect_is(lsm_l_enn_mn(landscape_stack), "tbl_df")
    expect_is(lsm_l_enn_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_enn_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_value), 6)
})

test_that("lsm_l_enn_mn returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_value$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$level, "character")
    expect_type(landscapemetrics_landscape_landscape_value$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_value$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_value$value, "double")
})



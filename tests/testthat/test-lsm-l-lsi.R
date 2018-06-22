context("landscape level lsi metric")

fragstats_landscape_landscape_lsi <- fragstats_landscape_landscape$lsi
landscapemetrics_landscape_landscape_lsi <- lsm_l_lsi(landscape)

test_that("lsm_l_lsi results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_lsi %in%
                        round(landscapemetrics_landscape_landscape_lsi$value, 4)))
})

test_that("lsm_l_lsi is typestable", {
    expect_is(landscapemetrics_landscape_landscape_lsi, "tbl_df")
    expect_is(lsm_l_lsi(landscape_stack), "tbl_df")
    expect_is(lsm_l_lsi(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_lsi returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_lsi), 6)
})

test_that("lsm_l_lsi returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_lsi$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$level, "character")
    expect_type(landscapemetrics_landscape_landscape_lsi$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_lsi$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_lsi$value, "double")
})

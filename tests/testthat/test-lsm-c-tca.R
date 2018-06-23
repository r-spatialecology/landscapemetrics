context("class level tca metric")

fragstats_class_landscape_tca <- fragstats_class_landscape$TCA
landscapemetrics_class_landscape_tca <- lsm_c_tca(landscape)

test_that("lsm_c_tca results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_tca %in%
                        round(landscapemetrics_class_landscape_tca$value, 4)))
})

test_that("lsm_c_tca is typestable", {
    expect_is(landscapemetrics_class_landscape_tca, "tbl_df")
    expect_is(lsm_c_tca(landscape_stack), "tbl_df")
    expect_is(lsm_c_tca(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_tca returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_tca), 6)
})

test_that("lsm_p_tca returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_tca$layer, "integer")
    expect_type(landscapemetrics_class_landscape_tca$level, "character")
    expect_type(landscapemetrics_class_landscape_tca$class, "integer")
    expect_type(landscapemetrics_class_landscape_tca$id, "integer")
    expect_type(landscapemetrics_class_landscape_tca$metric, "character")
    expect_type(landscapemetrics_class_landscape_tca$value, "double")
})



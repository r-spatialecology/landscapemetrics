context("class level circle_sd metric")

fragstats_class_landscape_circle_sd <- fragstats_class_landscape$CIRCLE_SD
landscapemetrics_class_landscape_circle_sd <- lsm_c_circle_sd(landscape)

test_that("lsm_c_circle_sd results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_circle_sd %in%
                        round(landscapemetrics_class_landscape_circle_sd$value, 4)))
})

test_that("lsm_c_circle_sd is typestable", {
    expect_is(landscapemetrics_class_landscape_circle_sd, "tbl_df")
    expect_is(lsm_c_circle_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_circle_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_circle returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_circle_sd), 6)
})

test_that("lsm_p_circle returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_circle_sd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_circle_sd$level, "character")
    expect_type(landscapemetrics_class_landscape_circle_sd$class, "integer")
    expect_type(landscapemetrics_class_landscape_circle_sd$id, "integer")
    expect_type(landscapemetrics_class_landscape_circle_sd$metric, "character")
    expect_type(landscapemetrics_class_landscape_circle_sd$value, "double")
})



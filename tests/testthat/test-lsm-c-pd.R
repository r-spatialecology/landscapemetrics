context("class level pd metric")

fragstats_class_landscape_pd <- fragstats_class_landscape$PD
landscapemetrics_class_landscape_pd <- lsm_c_pd(landscape)

test_that("lsm_p_area results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_pd %in%
                        round(landscapemetrics_class_landscape_pd$value,4)))
})

test_that("lsm_c_pd is typestable", {
    expect_is(landscapemetrics_class_landscape_pd, "tbl_df")
    expect_is(lsm_c_pd(landscape_stack), "tbl_df")
    expect_is(lsm_c_pd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_pd returns the desirpd number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_pd), 6)
})

test_that("lsm_p_pd returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_pd$layer, "integer")
    expect_type(landscapemetrics_class_landscape_pd$level, "character")
    expect_type(landscapemetrics_class_landscape_pd$class, "integer")
    expect_type(landscapemetrics_class_landscape_pd$id, "integer")
    expect_type(landscapemetrics_class_landscape_pd$metric, "character")
    expect_type(landscapemetrics_class_landscape_pd$value, "double")
})



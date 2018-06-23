context("class level np metric")

fragstats_class_landscape_np <- fragstats_class_landscape$NP
landscapemetrics_class_landscape_np <- lsm_c_np(landscape)

test_that("lsm_c_np results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_np %in%
                        round(landscapemetrics_class_landscape_np$value, 4)))
})

test_that("lsm_c_np is typestable", {
    expect_is(landscapemetrics_class_landscape_np, "tbl_df")
    expect_is(lsm_c_np(landscape_stack), "tbl_df")
    expect_is(lsm_c_np(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_np returns the desirnp number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_np), 6)
})

test_that("lsm_c_np returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_np$layer, "integer")
    expect_type(landscapemetrics_class_landscape_np$level, "character")
    expect_type(landscapemetrics_class_landscape_np$class, "integer")
    expect_type(landscapemetrics_class_landscape_np$id, "integer")
    expect_type(landscapemetrics_class_landscape_np$metric, "character")
    expect_type(landscapemetrics_class_landscape_np$value, "double")
})



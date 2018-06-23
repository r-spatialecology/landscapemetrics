context("class level pladj metric")

fragstats_class_landscape_pladj <- fragstats_class_landscape$PLADJ
landscapemetrics_class_landscape_pladj <- lsm_c_pladj(landscape)

test_that("lsm_c_pladj results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_pladj %in%
                        round(landscapemetrics_class_landscape_pladj$value, 4)))
})

test_that("lsm_c_pladj is typestable", {
    expect_is(landscapemetrics_class_landscape_pladj, "tbl_df")
    expect_is(lsm_c_pladj(landscape_stack), "tbl_df")
    expect_is(lsm_c_pladj(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_pladj returns the desirpladj number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_pladj), 6)
})

test_that("lsm_p_pladj returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_pladj$layer, "integer")
    expect_type(landscapemetrics_class_landscape_pladj$level, "character")
    expect_type(landscapemetrics_class_landscape_pladj$class, "integer")
    expect_type(landscapemetrics_class_landscape_pladj$id, "integer")
    expect_type(landscapemetrics_class_landscape_pladj$metric, "character")
    expect_type(landscapemetrics_class_landscape_pladj$value, "double")
})



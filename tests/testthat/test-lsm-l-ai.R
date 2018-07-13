context("class level ai metric")

fragstats_class_landscape_ai <- fragstats_class_landscape$AI
landscapemetrics_class_landscape_ai <- lsm_c_ai(landscape)

test_that("lsm_c_ai results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_ai %in%
                        round(landscapemetrics_class_landscape_ai$value, 4)))
})

test_that("lsm_c_ai is typestable", {
    expect_is(landscapemetrics_class_landscape_ai, "tbl_df")
    expect_is(lsm_c_ai(landscape_stack), "tbl_df")
    expect_is(lsm_c_ai(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_ai returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_ai), 6)
})

test_that("lsm_c_ai returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_ai$layer, "integer")
    expect_type(landscapemetrics_class_landscape_ai$level, "character")
    expect_type(landscapemetrics_class_landscape_ai$class, "integer")
    expect_type(landscapemetrics_class_landscape_ai$id, "integer")
    expect_type(landscapemetrics_class_landscape_ai$metric, "character")
    expect_type(landscapemetrics_class_landscape_ai$value, "double")
})



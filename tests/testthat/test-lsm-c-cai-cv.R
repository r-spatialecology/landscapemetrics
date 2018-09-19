context("class level cai_cv metric")

fragstats_class_landscape_value <- fragstats_patch_landscape %>%
    group_by(TYPE) %>%
    summarise(metric = raster::cv(CAI))

names(fragstats_class_landscape_value) <- c("class", "value")

landscapemetrics_class_landscape_value <- lsm_c_cai_cv(landscape)

comparison <- full_join(x = fragstats_class_landscape_value,
                        y = landscapemetrics_class_landscape_value,
                        by = "class",
                        suffix = c(".fs", ".lsm"))

test_that("lsm_c_cai_cv results are equal to fragstats", {
    expect_true(all(round(comparison$value.fs, 4) == round(comparison$value.lsm, 4)))
})

test_that("lsm_c_cai_cv is typestable", {
    expect_is(lsm_c_cai_cv(landscape), "tbl_df")
    expect_is(lsm_c_cai_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_cai_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_cai_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_cai_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})



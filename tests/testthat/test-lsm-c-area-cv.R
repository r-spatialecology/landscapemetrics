context("class level area cv metric")

# fragstats_class_landscape_area_cv <- fragstats_class_landscape$AREA_CV
fragstats_class_landscape_area_cv <- fragstats_patch_landscape %>%
    group_by(TYPE) %>%
    summarise(metric = raster::cv(AREA)) %>%
    pull(metric) %>%
    round(.,4)

landscapemetrics_class_landscape_area_cv <- lsm_c_area_cv(landscape)

test_that("lsm_c_area_cv results are equal to fragstats", {
    expect_true(all(fragstats_class_landscape_area_cv %in%
                        round(landscapemetrics_class_landscape_area_cv$value, 4)))
})

test_that("lsm_c_area_cv is typestable", {
    expect_is(landscapemetrics_class_landscape_area_cv, "tbl_df")
    expect_is(lsm_c_area_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_area_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_c_area_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_area_cv), 6)
})

test_that("lsm_c_area_cv returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_area_cv$layer, "integer")
    expect_type(landscapemetrics_class_landscape_area_cv$level, "character")
    expect_type(landscapemetrics_class_landscape_area_cv$class, "integer")
    expect_type(landscapemetrics_class_landscape_area_cv$id, "integer")
    expect_type(landscapemetrics_class_landscape_area_cv$metric, "character")
    expect_type(landscapemetrics_class_landscape_area_cv$value, "double")
})


context("landscape level shape_cv metric")

fragstats_landscape_landscape_shape_cv <- fragstats_patch_landscape %>%
    summarise(metric = raster::cv(SHAPE)) %>%
    pull(metric) %>%
    round(.,4)

#### FRAGSTATS rounds already the values on patch level, so we have to do the same for the test here
landscape_shape_cv <- landscape %>%
    lsm_p_shape_calc() %>%
    dplyr::summarize(metric = raster::cv(round(value,4), na.rm = TRUE))

test_that("lsm_l_shape_cv results are equal to fragstats", {
    expect_true(all(fragstats_landscape_landscape_shape_cv %in%
                        round(landscape_shape_cv$metric, 4)))
})

landscapemetrics_landscape_landscape_shape_cv <- lsm_l_shape_cv(landscape)

test_that("lsm_l_shape_cv is typestable", {
    expect_is(landscapemetrics_landscape_landscape_shape_cv, "tbl_df")
    expect_is(lsm_l_shape_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_shape_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_l_shape_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_landscape_landscape_shape_cv), 6)
})

test_that("lsm_l_shape_cv returns in every column the correct type", {
    expect_type(landscapemetrics_landscape_landscape_shape_cv$layer, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_cv$level, "character")
    expect_type(landscapemetrics_landscape_landscape_shape_cv$class, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_cv$id, "integer")
    expect_type(landscapemetrics_landscape_landscape_shape_cv$metric, "character")
    expect_type(landscapemetrics_landscape_landscape_shape_cv$value, "double")
})



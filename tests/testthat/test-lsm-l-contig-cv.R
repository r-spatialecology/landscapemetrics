context("landscape level contig metric")

fragstats_patch_landscape_contig_cv <- fragstats_patch_landscape %>%
    summarise(metric = raster::cv(CONTIG)) %>%
    pull(metric) %>%
    round(.,4)

#### FRAGSTATS rounds already the values on patch level, so we have to do the same for the test here
landscape_contig_cv <- landscape %>%
    lsm_p_contig_calc() %>%
    dplyr::summarize(metric = raster::cv(round(value,4), na.rm = TRUE))

test_that("lsm_p_contig_cv results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_contig_cv %in%
                        round(landscape_contig_cv$metric, 4)))
})

landscapemetrics_patch_landscape_contig_cv <- lsm_l_contig_cv(landscape)

test_that("lsm_p_contig_cv is typestable", {
    expect_is(landscapemetrics_patch_landscape_contig_cv, "tbl_df")
    expect_is(lsm_l_contig_cv(landscape_stack), "tbl_df")
    expect_is(lsm_l_contig_cv(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_contig_cv returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_contig_cv), 6)
})

test_that("lsm_p_contig_cv returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_contig_cv$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_cv$level, "character")
    expect_type(landscapemetrics_patch_landscape_contig_cv$class, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_cv$id, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_cv$metric, "character")
    expect_type(landscapemetrics_patch_landscape_contig_cv$value, "double")
})



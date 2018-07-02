context("class level contig metric")

fragstats_patch_landscape_contig_cv <- fragstats_class_landscape$CONTIG_CV
landscapemetrics_patch_landscape_contig_cv <- lsm_c_contig_cv(landscape)

test_that("lsm_p_contig_cv results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_contig_cv %in%
                        round(landscapemetrics_patch_landscape_contig_cv$value, 4)))
})

test_that("lsm_p_contig_cv is typestable", {
    expect_is(landscapemetrics_patch_landscape_contig_cv, "tbl_df")
    expect_is(lsm_c_contig_cv(landscape_stack), "tbl_df")
    expect_is(lsm_c_contig_cv(list(landscape, landscape)), "tbl_df")
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



context("class level contig metric")

fragstats_patch_landscape_contig_mn <- fragstats_class_landscape$CONTIG_MN
landscapemetrics_patch_landscape_contig_mn <- lsm_c_contig_mn(landscape)

test_that("lsm_p_contig_mn results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_contig_mn %in%
                        round(landscapemetrics_patch_landscape_contig_mn$value, 4)))
})

test_that("lsm_p_contig_mn is typestable", {
    expect_is(landscapemetrics_patch_landscape_contig_mn, "tbl_df")
    expect_is(lsm_c_contig_mn(landscape_stack), "tbl_df")
    expect_is(lsm_c_contig_mn(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_contig_mn returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_contig_mn), 6)
})

test_that("lsm_p_contig_mn returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_contig_mn$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_mn$level, "character")
    expect_type(landscapemetrics_patch_landscape_contig_mn$class, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_mn$id, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_mn$metric, "character")
    expect_type(landscapemetrics_patch_landscape_contig_mn$value, "double")
})



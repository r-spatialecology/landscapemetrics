context("class level contig metric")

fragstats_patch_landscape_contig_sd <- fragstats_class_landscape$CONTIG_SD
landscapemetrics_patch_landscape_contig_sd <- lsm_c_contig_sd(landscape)

test_that("lsm_p_contig_sd results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_contig_sd %in%
                        round(landscapemetrics_patch_landscape_contig_sd$value, 4)))
})

test_that("lsm_p_contig_sd is typestable", {
    expect_is(landscapemetrics_patch_landscape_contig_sd, "tbl_df")
    expect_is(lsm_c_contig_sd(landscape_stack), "tbl_df")
    expect_is(lsm_c_contig_sd(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_contig_sd returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_contig_sd), 6)
})

test_that("lsm_p_contig_sd returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_contig_sd$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_sd$level, "character")
    expect_type(landscapemetrics_patch_landscape_contig_sd$class, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_sd$id, "integer")
    expect_type(landscapemetrics_patch_landscape_contig_sd$metric, "character")
    expect_type(landscapemetrics_patch_landscape_contig_sd$value, "double")
})



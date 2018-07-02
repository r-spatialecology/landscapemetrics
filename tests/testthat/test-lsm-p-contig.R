context("patch level contig metric")

fragstats_patch_landscape_contig <- fragstats_patch_landscape$CONTIG
landscapemetrics_patch_landscape_contig <- lsm_p_contig(landscape)

test_that("lsm_p_contig results are equal to fragstats", {
    expect_true(all(fragstats_patch_landscape_contig %in%
                        round(landscapemetrics_patch_landscape_contig$value, 4)))
})

test_that("lsm_p_contig is typestable", {
    expect_is(landscapemetrics_patch_landscape_contig, "tbl_df")
    expect_is(lsm_p_contig(landscape_stack), "tbl_df")
    expect_is(lsm_p_contig(list(landscape, landscape)), "tbl_df")
})

test_that("lsm_p_contig returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_contig), 6)
})

test_that("lsm_p_contig returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_contig$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_contig$level, "character")
    expect_type(landscapemetrics_patch_landscape_contig$class, "integer")
    expect_type(landscapemetrics_patch_landscape_contig$id, "integer")
    expect_type(landscapemetrics_patch_landscape_contig$metric, "character")
    expect_type(landscapemetrics_patch_landscape_contig$value, "double")
})



landscapemetrics_patch_landscape_value <- lsm_p_contig(landscape)

test_that("lsm_p_contig is typestable", {
    expect_s3_class(lsm_p_contig(landscape), "tbl_df")
    expect_s3_class(lsm_p_contig(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_contig(landscape_list), "tbl_df")
})

test_that("lsm_p_contig returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_contig returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_contig equals FRAGSTATS", {
    lsm_landscape <- lsm_p_contig(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_contig(augusta_nlcd) |> dplyr::pull(value)

    fs_landscape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "contig") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "contig") |> dplyr::pull(value)

    expect_true(test_correlation(obs = lsm_landscape, exp = fs_landscape, tolerance = tol_cor))
    expect_true(test_correlation(obs = lsm_augusta, exp = fs_augusta, tolerance = tol_cor))
})

result <- lsm_p_perim(landscape)

test_that("lsm_p_perim is typestable", {
    expect_s3_class(lsm_p_perim(landscape), "tbl_df")
    expect_s3_class(lsm_p_perim(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_perim(landscape_list), "tbl_df")
})

test_that("lsm_p_perim returns the desired number of columns", {
    expect_equal(ncol(result), 6)
})

test_that("lsm_p_perim returns in every column the correct type", {
    expect_type(result$layer, "integer")
    expect_type(result$level, "character")
    expect_type(result$class, "integer")
    expect_type(result$id, "integer")
    expect_type(result$metric, "character")
    expect_type(result$value, "double")
})

test_that("lsm_p_perim can also handle irregular sized cells", {
    expect_s3_class(lsm_p_perim(landscape_diff_res), "tbl_df")
})

test_that("lsm_p_perim equals FRAGSTATS", {
    lsm_landscape <- lsm_p_perim(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_p_perim(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "perim") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "perim") |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape))
    expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta))
})

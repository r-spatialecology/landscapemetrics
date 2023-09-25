landscapemetrics_class_landscape_value <- lsm_c_np(landscape)

test_that("lsm_c_np is typestable", {
    expect_s3_class(lsm_c_np(landscape), "tbl_df")
    expect_s3_class(lsm_c_np(landscape_stack), "tbl_df")
    expect_s3_class(lsm_c_np(landscape_list), "tbl_df")
})

test_that("lsm_c_np returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_class_landscape_value), 6)
})

test_that("lsm_c_np returns in every column the correct type", {
    expect_type(landscapemetrics_class_landscape_value$layer, "integer")
    expect_type(landscapemetrics_class_landscape_value$level, "character")
    expect_type(landscapemetrics_class_landscape_value$class, "integer")
    expect_type(landscapemetrics_class_landscape_value$id, "integer")
    expect_type(landscapemetrics_class_landscape_value$metric, "character")
    expect_type(landscapemetrics_class_landscape_value$value, "double")
})

test_that("lsm_c_np equals FRAGSTATS", {
    lsm_landscape <- lsm_c_np(landscape) |> dplyr::pull(value)
    lsm_augusta <- lsm_c_np(augusta_nlcd) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_class, LID == "landscape", metric == "np") |> dplyr::pull(value)
    fs_augusta <- dplyr::filter(fragstats_class, LID == "augusta_nlcd", metric == "np") |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape))
    expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta))
})

landscapemetrics_patch_landscape_value <- lsm_p_circle(landscape)

test_that("lsm_p_circle is typestable", {
    expect_s3_class(lsm_p_circle(landscape), "tbl_df")
    expect_s3_class(lsm_p_circle(landscape_stack), "tbl_df")
    expect_s3_class(lsm_p_circle(landscape_list), "tbl_df")
})

test_that("lsm_p_circle returns the desired number of columns", {
    expect_equal(ncol(landscapemetrics_patch_landscape_value), 6)
})

test_that("lsm_p_circle returns in every column the correct type", {
    expect_type(landscapemetrics_patch_landscape_value$layer, "integer")
    expect_type(landscapemetrics_patch_landscape_value$level, "character")
    expect_type(landscapemetrics_patch_landscape_value$class, "integer")
    expect_type(landscapemetrics_patch_landscape_value$id, "integer")
    expect_type(landscapemetrics_patch_landscape_value$metric, "character")
    expect_type(landscapemetrics_patch_landscape_value$value, "double")
})

test_that("lsm_p_circle can also handle irregular sized cells", {
    expect_error(object = lsm_p_circle(landscape_diff_res),
                 regexp = "The area of the circumscribing circle is currently only implemented for equal resolutions.")
})

# https://fragstats.org/index.php/fragstats-metrics/shape-metrics/p4-related-circumscribing-circle
# In addition, the index never quite equals 0 because the grid data format doesn't allow patches to be perfectly circular

# MH: Augusta is off quite a bit for smaller patches

test_that("lsm_p_circle equals FRAGSTATS", {

    lsm_landscape <- lsm_p_circle(landscape) |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
    # lsm_augusta <- lsm_p_circle(augusta_nlcd) |> dplyr::filter(value != min(value)) |> dplyr::pull(value)

    fs_landcape <- dplyr::filter(fragstats_patch, LID == "landscape", metric == "circle") |> dplyr::filter(value != min(value)) |> dplyr::pull(value)
    # fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric == "circle") |> dplyr::filter(value != min(value)) |> dplyr::pull(value)

    expect_equal(object = sort(lsm_landscape), expected = sort(fs_landcape), tolerance = 0.01)
    # expect_equal(object = sort(lsm_augusta), expected = sort(fs_augusta), tolerance = 0.01)
})

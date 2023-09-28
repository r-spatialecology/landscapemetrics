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

# MH: Something is wrong here

test_that("lsm_p_circle equals FRAGSTATS", {
    lsm_landscape <- calculate_lsm(landscape, what = c("lsm_p_area", "lsm_p_circle")) |>
        tidyr::pivot_wider(names_from = metric, values_from = value) |>
        dplyr::filter(area != min(area)) |>
        dplyr::pull(circle)

    lsm_augusta <- calculate_lsm(augusta_nlcd, what = c("lsm_p_area", "lsm_p_circle")) |>
        tidyr::pivot_wider(names_from = metric, values_from = value) |>
        dplyr::filter(area != min(area)) |>
        dplyr::pull(circle)

    fs_landscape <- dplyr::filter(fragstats_patch, LID == "landscape", metric %in% c("area", "circle")) |>
        tidyr::pivot_wider(names_from = metric, values_from = value) |>
        dplyr::filter(area != min(area)) |>
        dplyr::pull(circle)

    fs_augusta <- dplyr::filter(fragstats_patch, LID == "augusta_nlcd", metric %in% c("area", "circle")) |>
        tidyr::pivot_wider(names_from = metric, values_from = value) |>
        dplyr::filter(area != min(area)) |>
        dplyr::pull(circle)

    expect_true(test_diff(obs = lsm_landscape, exp = fs_landscape, tol = tolerance))
    expect_true(test_diff(obs = lsm_augusta, exp = fs_augusta, tol = tolerance))
})

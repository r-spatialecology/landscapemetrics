test_that("extract_lsm returns correct metrics", {

    patch_area <- extract_lsm(landscape, y = sample_points,
                              what = "lsm_p_area", verbose = FALSE)

    expect_true(all(patch_area$metric == "area"))

    patch_core <- extract_lsm(landscape, y = sample_points,
                              type = "core area metric", full_name = TRUE,
                              verbose = FALSE)

    expect_true(all(patch_core$type == "core area metric"))

    patch_all <- extract_lsm(landscape, y = sample_points,
                             verbose = FALSE)

    expect_true(all(unique(patch_all$metric) == list_lsm(level = "patch")[,1]))
})

# test_that("extract_lsm works for lines", {
#
#     result <- extract_lsm(landscape,
#                           y = sample_lines,
#                           what = "lsm_p_area",
#                           verbose = FALSE)
#
#     expect_s3_class(result, "tbl_df")
#     expect_true(all(result$metric %in% "area"))
# })

test_that("extract_lsm forwards arguments to calculate_lsm", {

    result <- extract_lsm(landscape, y = sample_points,
                          what = "lsm_p_core", edge_depth = 100,
                          verbose = FALSE)

    expect_true(all(result$value == 0))
})

test_that("extract_lsm uses extract_ids", {

    result <- extract_lsm(landscape, y = sample_points,
                          extract_id = c(5, 25, 15), what = "lsm_p_area",
                          verbose = FALSE)

    expect_equal(result$extract_id, expected = c(5, 15, 25))

    result_wrong_id <- extract_lsm(landscape,  y = sample_points,
                                   extract_id = c(1, 5, 25, 15),
                                   what = "lsm_p_area", verbose = FALSE)

    expect_equal(result_wrong_id$extract_id, expected = 1:3)
})

test_that("extract_lsm works for all data types", {

    result_stack <- extract_lsm(landscape = landscape_stack, y = sample_points,
                                what = "lsm_p_area", verbose = FALSE)

    result_list <- extract_lsm(landscape = landscape_list, y = sample_points,
                               what = "lsm_p_area", verbose = FALSE)

    expect_s3_class(result_stack, "tbl_df")
    expect_s3_class(result_list, "tbl_df")

    expect_equal(object = result_stack$layer,
                 expected = c(1, 1, 1, 2, 2, 2))
    expect_equal(object = result_list$layer,
                 expected = c(1, 1, 1, 2, 2, 2))

    expect_true("area" %in% result_stack$metric)
    expect_true("area" %in% result_list$metric)
})

test_that("extract_lsm returns warnings", {

    expect_warning(extract_lsm(landscape, y = sample_points,
                               extract_id = c(15, 25, 5, 1), what = "lsm_p_area"),
                   regexp = "Length of extract_id is not identical to length of y. Using 1...n as extract_id.",
                   fixed = TRUE)
})

test_that("extract_lsm throws errors", {

    expect_error(extract_lsm(landscape, y = sample_points,
                             what = "lsm_l_ta", verbose = FALSE),
                 regexp = "'extract_lsm()' only takes patch level metrics.",
                 fixed = TRUE)

    expect_error(extract_lsm(landscape, y = 1:3),
                 regexp = "'y' must be a matrix, SpatVector, or sf object.",
                 fixed = TRUE)
})


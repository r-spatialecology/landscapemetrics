context("sample_lsm")

test_that("sample_lsm works for a matrix", {

    result_mat <- sample_lsm(landscape,
                             y = sample_points, size = 5,
                             shape = "circle",
                             what = c("lsm_l_ta", "lsm_l_np"),
                             verbose = FALSE)

    expect_is(object = result_mat, class = "tbl_df")
    expect_true(all(c("np", "ta") %in% result_mat$metric))
})

test_that("sample_lsm works for sf points", {

    result_sp <- sample_lsm(landscape,
                            y = points_sf, size = 5,
                            shape = "square",
                            what = "lsm_l_np",
                            verbose = FALSE)

    expect_is(object = result_sp, class = "tbl_df")

    expect_true(all(c("np") %in% result_sp$metric))
})

# test_that("sample_lsm works for polygons ", {
#
#     result_poly <- sample_lsm(landscape,
#                               y = sample_plots, size = 5,
#                               level = "patch",
#                               verbose = FALSE)
#
#     expect_is(object = result_poly, class = "tbl_df")
#
#     expect_true(all("patch" %in% result_poly$level))
#
# })

# test_that("sample_lsm works for lines ", {
#
#     result_lines <- sample_lsm(landscape,
#                                y = sample_lines,
#                                size = 5,
#                                level = "landscape",
#                                verbose = FALSE)
#
#     expect_is(object = result_lines, class = "tbl_df")
#
#     expect_true(all("landscape" %in% result_lines$level))
# })

test_that("sample_lsm forwards arguments to calculate_lsm", {

    result_mat <- sample_lsm(landscape,
                             y = sample_points, size = 5,
                             shape = "circle",
                             what = "lsm_p_core",
                             edge_depth = 100,
                             verbose = FALSE)

    expect_true(all(result_mat$value == 0))
})

test_that("sample_lsm uses sample_ids", {

    result <- sample_lsm(landscape,
                         y = sample_points,
                         plot_id = c(5, 25, 15),
                         size = 15,
                         shape = "circle",
                         what = "lsm_l_ta",
                         verbose = FALSE)

    expect_equal(result$plot_id, expected = c(5, 15, 25))

    result_wrong_id <- sample_lsm(landscape,
                                  y = sample_points,
                                  plot_id = c(5, 25, 15, 1),
                                  size = 15,
                                  shape = "circle",
                                  what = "lsm_l_ta",
                                  verbose = FALSE)

    expect_equal(result_wrong_id$plot_id, expected = 1:3)
})


test_that("sample_lsm can return all classes", {

    result_mat <- sample_lsm(landscape,
                             y = sample_points,
                             size = 5,
                             shape = "circle",
                             what = c("lsm_c_ca", "lsm_l_ta"),
                             all_classes = TRUE,
                             verbose = FALSE)

    # 3 classes * 3 sample plots + 3 landscape level metrics
    expect_equal(object = nrow(result_mat), expected = 12)

    expect_equal(object = result_mat$class, expected = c(1, 2, 3, NA,
                                                         1, 2, 3, NA,
                                                         1, 2, 3, NA))
})


test_that("sample_lsm works for all data type", {

    result_stack <- sample_lsm(landscape_stack,
                               y = sample_points,
                               size = 5,
                               what = "lsm_l_ta",
                               verbose = FALSE)

    result_list <- sample_lsm(landscape_list,
                              y = sample_points,
                              size = 5,
                              what = "lsm_l_ta",
                              verbose = FALSE)

    expect_is(result_stack, class = "tbl_df")
    expect_is(result_list, class = "tbl_df")

    expect_equal(object = result_stack$layer,
                 expected = c(1, 1, 1, 2, 2, 2))
    expect_equal(object = result_list$layer,
                 expected = c(1, 1, 1, 2, 2, 2))

    expect_true("ta" %in% result_stack$metric)
    expect_true("ta" %in% result_list$metric)

})

test_that("sample_lsm returns errors", {

    expect_error(sample_lsm(landscape,
                            y = 1:3,
                            size = 5),
                 regexp = "Please provide a matrix with coords, points or polygons object.",
                 fixed = TRUE)

    expect_error(sample_lsm(landscape,
                            y = sample_points,
                            size = c(5, 15),
                            what = "lsm_l_ta"),
                 regexp = "Please provide only one value as size argument (size > 0).",
                 fixed = TRUE)
})

test_that("sample_lsm returns warnings", {

    expect_warning(sample_lsm(landscape,
                              y = sample_points, size = 50, what = "lsm_l_ta"),
                   regexp = "The 'perecentage_inside' is below 90% for at least one buffer.",
                   fixed = TRUE)

    expect_warning(sample_lsm(landscape,
                              y = sample_points,
                              plot_id = c(5, 25, 15, 1),
                              size = 15,
                              shape = "circle",
                              what = "lsm_l_ta"),
                   regexp = "Length of plot_id is not identical to length of y. Using 1...n as plot_id.",
                   fixed = TRUE)
})

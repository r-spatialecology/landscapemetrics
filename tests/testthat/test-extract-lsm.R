context("extract_lsm")

sample_points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)

x1 <- c(1, 5, 15, 10)
y1 <- c(1, 5, 15, 25)

x2 <- c(10, 25)
y2 <- c(5, 5)

sample_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(x1, y1)),
                                                     sp::Line(cbind(x2, y2))), ID = "a")))

test_that("extract_lsm returns correct metrics", {

    patch_area <- extract_lsm(landscape,
                              y = sample_points,
                              what = "lsm_p_area",
                              type = "aggregation metric",
                              verbose = FALSE)

    expect_true(all(patch_area$metric == "area"))

    patch_core <- extract_lsm(landscape,
                              y = sample_points,
                              type = "core area metric",
                              full_name = TRUE,
                              verbose = FALSE)

    expect_true(all(patch_core$type == "core area metric"))

    patch_all <- extract_lsm(landscape,
                             y = sample_points,
                             verbose = FALSE)

    expect_true(all(unique(patch_all$metric) == list_lsm(level = "patch")[,1]))
})

test_that("extract_lsm works for lines", {

    result <- extract_lsm(landscape,
                          y = sample_lines,
                          what = "lsm_p_area",
                          verbose = FALSE)

    expect_is(result, "tbl_df")
    expect_true(all(result$metric %in% "area"))
})

test_that("extract_lsm forwards arguments to calculate_lsm", {

    result <- extract_lsm(landscape,
                          y = sample_points,
                          what = "lsm_p_core",
                          edge_depth = 100,
                          verbose = FALSE)

    expect_true(all(result$value == 0))
})

test_that("extract_lsm uses extract_ids", {

    result <- extract_lsm(landscape,
                          y = sample_points,
                          extract_id = c(5, 25, 15),
                          what = "lsm_p_area",
                          verbose = FALSE)

    expect_equal(result$extract_id, expected = c(5, 15, 25))

    result_wrong_id <- extract_lsm(landscape,
                                   y = sample_points,
                                   extract_id = c(1, 5, 25, 15),
                                   what = "lsm_p_area",
                                   verbose = FALSE)

    expect_equal(result_wrong_id$extract_id, expected = 1:3)
})

test_that("extract_lsm works for all data types", {

    result_stack <- extract_lsm(landscape = landscape_stack,
                                y = sample_points,
                                what = "lsm_p_area",
                                verbose = FALSE)

    result_brick <- extract_lsm(landscape = landscape_brick,
                                y = sample_points,
                                what = "lsm_p_area",
                                verbose = FALSE)

    result_list <- extract_lsm(landscape = landscape_list,
                               y = sample_points,
                               what = "lsm_p_area",
                               verbose = FALSE)

    expect_is(result_stack, "tbl_df")
    expect_is(result_brick, "tbl_df")
    expect_is(result_list, "tbl_df")

    expect_equal(object = result_stack$layer,
                 expected = c(1, 1, 1, 2, 2, 2))
    expect_equal(object = result_brick$layer,
                 expected = c(1, 1, 1, 2, 2, 2))
    expect_equal(object = result_list$layer,
                 expected = c(1, 1, 1, 2, 2, 2))

    expect_true("area" %in% result_stack$metric)
    expect_true("area" %in% result_brick$metric)
    expect_true("area" %in% result_list$metric)
})

test_that("extract_lsm returns warnings", {

    expect_warning(extract_lsm(landscape,
                               y = sample_points,
                               extract_id = c(15, 25, 5, 1),
                               what = "lsm_p_area"),
                   regexp = "Length of extract_id is not identical to length of y. Using 1...n as extract_id.",
                   fixed = TRUE)
})

test_that("extract_lsm throws errors", {

    expect_error(extract_lsm(landscape,
                             y = sample_points,
                             what = "lsm_l_ta",
                             verbose = FALSE),
                 regexp = "'extract_lsm()' only takes patch level metrics.",
                 fixed = TRUE)

    expect_error(extract_lsm(landscape, y = 1:3),
                 regexp = "'y' must be a matrix, SpatialPoints, SpatialLines or sf point geometries.",
                 fixed = TRUE)
})


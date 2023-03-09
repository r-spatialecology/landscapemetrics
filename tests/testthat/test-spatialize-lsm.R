context("spatialize_lsm")

test_that("spatialize_lsm returns all selected metrics", {

    result <- spatialize_lsm(landscape, what = c("lsm_p_area",
                                                 "lsm_p_contig",
                                                 "lsm_p_perim"),
                             verbose = FALSE)

    expect_length(object = result, n = 1)

    expect_equal(object = names(result[[1]]),

                 expected = c("lsm_p_area", "lsm_p_contig", "lsm_p_perim"))

    expect_true(object = all(sapply(result[[1]], class) == "SpatRaster"))
})

test_that("spatialize_lsm returns returns correct type of metrics", {

    metrics <- list_lsm(level = "patch", type = "shape metric", simplify = TRUE)

    result <- spatialize_lsm(landscape, type = "shape metric",
                      verbose = FALSE)

    result_all <- spatialize_lsm(landscape, verbose = FALSE)

    expect_equal(object = names(result[[1]]),
                 expected = metrics)

    expect_equal(object = names(result_all[[1]]),
                 expected = list_lsm(level = "patch", simplify = TRUE))
})

test_that("spatialize_lsm returns CRS", {

    result <- spatialize_lsm(podlasie_ccilc, what = "lsm_p_area",
                             verbose = FALSE)

    expect_equal(object = terra::crs(result[[1]][[1]], proj = TRUE),
                 expected = terra::crs(podlasie_ccilc, proj = TRUE))
})

test_that("spatialize_lsm forwards arguments to calculate_lsm", {

    result <- spatialize_lsm(landscape, what = "lsm_p_core",
                             verbose = FALSE, edge_depth = 10)

    expect_true(all(result[[1]][[1]][] == 0))
})

test_that("spatialize_lsm works for all data types", {

    expect_length(object = spatialize_lsm(landscape_stack,
                                          what = "lsm_p_area",
                                          verbose = FALSE), n = 2)

    expect_length(object = spatialize_lsm(list(landscape, landscape),
                                          what = "lsm_p_area",
                                          verbose = FALSE), n = 2)
})

test_that("spatialize_lsm uses temp file", {

    result <- spatialize_lsm(landscape, what = "lsm_p_area",
                             to_disk = TRUE,
                             verbose = FALSE)

    expect_false(terra::inMemory(result[[1]]$lsm_p_area))
})


test_that("spatialize_lsm returns all errors", {

    expect_error(spatialize_lsm(landscape, what = "lsm_l_ta",
                         verbose = FALSE),
                 regexp = "'spatialize_lsm()' only takes patch level metrics.",
                 fixed = TRUE)
})

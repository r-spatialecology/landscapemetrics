context("get_lsm")

test_that("get_lsm returns all selected metrics", {

    result <- get_lsm(landscape, what = c("lsm_p_area",
                                          "lsm_p_contig",
                                          "lsm_p_perim"),
                      verbose = FALSE)

    expect_length(object = result, n = 1)
    expect_equal(object = names(result[[1]]),
                 expected = c("lsm_p_area", "lsm_p_contig", "lsm_p_perim"))
    expect_true(object = all(sapply(result[[1]], class) == "RasterLayer"))
})

test_that("get_lsm returns returns correct type of metrics", {

    metrics <- list_lsm(level = "patch", type = "shape metric", simplify = TRUE)

    result <- get_lsm(landscape, type = "shape metric",
                      verbose = FALSE)

    result_all <- get_lsm(landscape, verbose = FALSE)

    expect_equal(object = names(result[[1]]),
                 expected = metrics)

    expect_equal(object = names(result_all[[1]]),
                 expected = list_lsm(level = "patch", simplify = TRUE))
})

test_that("get_lsm returns returns CRS", {

    result <- get_lsm(podlasie_ccilc, what = "lsm_p_area",
                      verbose = FALSE)

    expect_equal(object = raster::crs(result[[1]][[1]]),
                 expected = raster::crs(podlasie_ccilc))
})

test_that("get_lsm works for all data types", {

    expect_length(object = get_lsm(landscape_stack, what = "lsm_p_area",
                                   verbose = FALSE),
                  n = 2)

    expect_length(object = get_lsm(landscape_brick, what = "lsm_p_area",
                                   verbose = FALSE),
                  n = 2)

    expect_length(object = get_lsm(list(landscape, landscape), what = "lsm_p_area",
                                   verbose = FALSE),
                  n = 2)
})


test_that("get_lsm returns all errors", {

    expect_error(get_lsm(landscape, level = "landscape",
                         verbose = FALSE),
                 grep = "Please provide (at least one) patch level metrics only.
                 To list available metrics, run list_lsm(level = 'patch').",
                 fixed = TRUE)
})

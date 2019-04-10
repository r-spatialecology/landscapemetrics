context("extract_lsm")

points <- raster::sampleRandom(landscape, 20, sp = TRUE)
res_extract_raster <- extract_lsm(landscape, points)
res_extract_rasterstack <- extract_lsm(landscape_stack, points)
res_extract_rasterbrick <- extract_lsm(landscape_brick, points)
res_extract_rasterlist <- extract_lsm(landscape_list, points)

test_that("extract_lsm returns a tbl", {
    expect_is(res_extract_rasterlist, "tbl_df")
    expect_is(res_extract_rasterstack, "tbl_df")
    expect_is(res_extract_rasterbrick, "tbl_df")
    expect_is(res_extract_rasterlist, "tbl_df")
})

test_that("extract_lsm works for lines", {

    x1 <- c(1, 5, 15, 10)
    y1 <- c(1, 5, 15, 25)

    x2 <- c(10, 25)
    y2 <- c(5, 5)

    sample_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(x1, y1)),
                                                         sp::Line(cbind(x2, y2))), ID = "a")))

    result <- extract_lsm(landscape, y = sample_lines, what = "lsm_p_area")

    expect_is(result, "tbl_df")
    expect_true(all(result$metric %in% "area"))
})

test_that("extract_lsm returns correct metrics", {

    patch_area <- extract_lsm(landscape, points,
                              what = "lsm_p_area",
                              type = "aggregation metric")

    expect_true(all(patch_area$metric == "area"))

    patch_core <- extract_lsm(landscape, points,
                              type = "core area metric",
                              full_name = TRUE)

    expect_true(all(patch_core$type == "core area metric"))

   patch_all <- extract_lsm(landscape, points)

   expect_true(all(unique(patch_all$metric) == list_lsm(level = "patch")[,1]))
})


test_that("extract_lsm forwards arguments to calculate_lsm", {

    result <- extract_lsm(landscape, points,
                          what = "lsm_p_core",
                          edge_depth = 100)

    expect_true(all(result$value == 0))
})

test_that("extract_lsm throws errors", {

    expect_error(extract_lsm(landscape, y = points, what = "lsm_l_area"),
                 regexp = "extract_lsm only takes patch level metrics as what argument.")

    expect_error(extract_lsm(landscape, y = 1:3),
                 regexp = "'y' must be a matrix, SpatialPoints, SpatialLines or sf point geometries.")
})


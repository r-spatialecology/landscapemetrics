# context("pad_raster")
#
# test_that("pad_raster can handle all raster inputs", {
#     expect_is(landscapemetrics:::pad_raster(landscape), "RasterLayer")
#     expect_is(landscapemetrics:::pad_raster(landscape_stack), "list")
#     expect_is(landscapemetrics:::pad_raster(landscape_brick), "list")
#     expect_is(landscapemetrics:::pad_raster(landscape_list), "list")
# })
#
# test_that("pad_raster can handle options", {
#     expect_is(landscapemetrics:::pad_raster(landscape,
#                          pad_raster_value = -5,
#                          pad_raster_cells = 5,
#                          global = TRUE), "RasterLayer")
# })

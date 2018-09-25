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

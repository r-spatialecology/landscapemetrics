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

test_that("extract_lsm returns correct metrics", {

    patch_area <- extract_lsm(landscape, points, what = "lsm_p_area", type = "aggregation metric")
    expect_true(all(patch_area$metric == "area"))

    patch_core <- extract_lsm(landscape, points, type = "core area metric",
                              full_name = TRUE)
    expect_true(all(patch_core$type == "core area metric"))

   patch_all <- extract_lsm(landscape, points)
   expect_true(all(unique(patch_all$metric) == list_lsm(level = "patch")[,1]))
})

test_that("extract_lsm throws error for non-patch metrics", {

    expect_error(extract_lsm(landscape, points, what = "lsm_l_area"),
                 regexp = "extract_lsm only takes patch level metrics as what argument.")
})

context("calculate_lsm")

test_that("calculate_lsm can calculate patch metrics", {
    patch_metrics <- calculate_lsm(landscape, what = "patch")
    expect_is(patch_metrics, "tbl_df")
    expect_true(ncol(patch_metrics) > 0)
})

test_that("calculate_lsm can calculate class metrics", {
    class_metrics <- calculate_lsm(landscape, what = "class")
    expect_is(class_metrics, "tbl_df")
    expect_true(ncol(class_metrics) > 0)
})


test_that("calculate_lsm can calculate landscape metrics", {
    landscape_metrics <- calculate_lsm(landscape, what = "landscape")
    expect_is(landscape_metrics, "tbl_df")
    expect_true(ncol(landscape_metrics) > 0)
})

test_that("calculate_lsm can take all metrics", {
    all_metrics <- calculate_lsm(landscape, what = "all")
    expect_is(all_metrics, "tbl_df")
    expect_true(ncol(all_metrics) > 0)
})

test_that("calculate_lsm can take specific metrics", {
    specific_metrics <- calculate_lsm(landscape, what = c("lsm_p_enn", "lsm_c_ed"))
    expect_is(specific_metrics, "tbl_df")
    expect_true(ncol(specific_metrics) > 0)
})

test_that("calculate_lsm can take specific metrics and multiple arguments", {
    specific_metrics <- calculate_lsm(landscape, what = c("lsm_p_enn", "lsm_c_ed"),
                                     count_boundary = TRUE, directions = 4,
                                     progress = TRUE, full_name = TRUE)
    expect_is(specific_metrics, "tbl_df")
    expect_true(ncol(specific_metrics) > 0)
})

test_that("calculate_lsm can take different raster inputs", {
    expect_is(calculate_lsm(landscape, what = "lsm_l_ta", progress = TRUE), "tbl_df")
    expect_is(calculate_lsm(landscape_stack, what = "lsm_l_ta", progress = TRUE), "tbl_df")
    expect_is(calculate_lsm(landscape_brick, what = "lsm_l_ta", progress = TRUE), "tbl_df")
    expect_is(calculate_lsm(landscape_list, what = "lsm_l_ta", progress = TRUE), "tbl_df")
})

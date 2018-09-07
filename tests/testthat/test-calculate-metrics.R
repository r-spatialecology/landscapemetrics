context("calculate_lsm")

test_that("calculate_lsm can calculate patch metrics", {
    patches <- calculate_lsm(landscape, what = "patch")
    expect_is(patches, "tbl_df")
    expect_true(ncol(patches) > 0)
})

test_that("calculate_lsm can calculate class metrics", {
    classes <- calculate_lsm(landscape, what = "class")
    expect_is(classes, "tbl_df")
    expect_true(ncol(classes) > 0)
})


test_that("calculate_lsm can calculate landscape metrics", {
    landscapes <- calculate_lsm(landscape, what = "landscape")
    expect_is(landscapes, "tbl_df")
    expect_true(ncol(landscapes) > 0)
})

test_that("calculate_lsm can take specific metrics", {
    specificmetrics <- calculate_lsm(landscape, what = c("lsm_p_enn", "lsm_c_ed"))
    expect_is(specificmetrics, "tbl_df")
    expect_true(ncol(specificmetrics) > 0)
})

test_that("calculate_lsm can take specific metrics and multiple arguments", {
    specificmetrics <- calculate_lsm(landscape, what = c("lsm_p_enn", "lsm_c_ed"), count_boundary = TRUE, directions = 4)
    expect_is(specificmetrics, "tbl_df")
    expect_true(ncol(specificmetrics) > 0)
})

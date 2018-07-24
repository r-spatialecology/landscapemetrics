context("calculate_metrics")

test_that("calculate_metrics can calculate patch metrics", {
    patches <- calculate_metrics(landscape, what = "patch")
    expect_is(patches, "tbl_df")
    expect_true(ncol(patches) > 0)
})

test_that("calculate_metrics can calculate class metrics", {
    classes <- calculate_metrics(landscape, what = "class")
    expect_is(classes, "tbl_df")
    expect_true(ncol(classes) > 0)
})


test_that("calculate_metrics can calculate landscape metrics", {
    landscapes <- calculate_metrics(landscape, what = "landscape")
    expect_is(landscapes, "tbl_df")
    expect_true(ncol(landscapes) > 0)
})

test_that("calculate_metrics can take specific metrics", {
    specificmetrics <- calculate_metrics(landscape, what = c("lsm_p_enn", "lsm_c_ed"))
    expect_is(specificmetrics, "tbl_df")
    expect_true(ncol(specificmetrics) > 0)
})

test_that("calculate_metrics can take specific metrics and multiple arguments", {
    specificmetrics <- calculate_metrics(landscape, what = c("lsm_p_enn", "lsm_c_ed"), count_boundary = TRUE, directions = 4)
    expect_is(specificmetrics, "tbl_df")
    expect_true(ncol(specificmetrics) > 0)
})

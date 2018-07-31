context("show_patches")

metrics_patch <- calculate_metrics(landscape, what = 'patch')
metrics_plot_patch <- show_correlation(metrics_patch, method = "pearson")

metrics_class <- calculate_metrics(landscape, what = 'class')
metrics_plot_class <- show_correlation(metrics_class, method = "pearson")

metrics_mult <- calculate_metrics(landscape, what = c("patch", "class"))
metrics_plot_mult <- show_correlation(metrics_mult, method = "pearson")

test_that("show_patches returns a plot", {
    expect_equal(class(metrics_plot_patch), c("gg","ggplot"))
})
test_that("show_patches returns a plot", {
    expect_equal(class(metrics_plot_class), c("gg","ggplot"))
})

test_that("show_patches returns a plot", {
    expect_equal(class(metrics_plot_mult), c("gg","ggplot"))
})

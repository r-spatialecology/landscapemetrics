context("show_patches")

metrics_patch <- calculate_metrics(landscape, what = 'patch')
metrics_plot_patch <- show_correlation(metrics_patch, level = "patch", method = "pearson")

metrics_class <- calculate_metrics(landscape, what = 'class')
metrics_plot_class <- show_correlation(metrics_class, level = "class", method = "pearson")

# metrics_landscape <- calculate_metrics(landscape, what = c("class", "patch"))
# metrics_plot_landscape <- show_correlation(metrics_landscape, level = c("class", "patch"), method = "pearson")
#

test_that("show_patches returns a plot", {
    expect_equal(class(metrics_plot_patch), c("gg","ggplot"))
})
test_that("show_patches returns a plot", {
    expect_equal(class(metrics_plot_class), c("gg","ggplot"))
})
# test_that("show_patches returns a plot", {
#     expect_equal(class(metrics_plot_landscape), c("gg","ggplot"))
# })

context("show_correlation")

metrics_patch <- calculate_lsm(landscape, what = 'patch')
metrics_plot_patch <- show_correlation(metrics_patch, method = "pearson")

metrics_class <- calculate_lsm(landscape, what = 'class')
metrics_plot_class <- show_correlation(metrics_class, method = "pearson")

metrics_mult <- calculate_lsm(landscape, what = c("patch", "class"))
metrics_plot_mult <- show_correlation(metrics_mult, method = "pearson")
correlations <- calculate_correlation(metrics_mult)
corr_plot <- show_correlation(correlations)

metrics_land <- calculate_lsm(landscape_stack, level = "landscape",
                              type = "area and edge metric")
metrics_plot_land <- show_correlation(metrics_land, method = "pearson")

test_that("show_correlation returns a plot on patch level", {
    expect_equal(class(metrics_plot_patch), c("gg","ggplot"))
})
test_that("show_correlation returns a plot on class level", {
    expect_equal(class(metrics_plot_class), c("gg","ggplot"))
})

test_that("show_correlation returns a plot for patch and class level", {
    expect_equal(class(metrics_plot_mult), c("gg","ggplot"))
})

test_that("show_correlation returns a plot for landscape level", {
    expect_equal(class(metrics_plot_land), c("gg","ggplot"))
})

test_that("show_correlation returns a plot for correlation list", {
    expect_equal(class(corr_plot), c("gg","ggplot"))
})

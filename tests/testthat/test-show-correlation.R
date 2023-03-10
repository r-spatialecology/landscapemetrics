context("show_correlation")

# patch level
metrics_patch <- calculate_lsm(landscape,
                               what = 'patch',
                               verbose = FALSE)

metrics_plot_patch <- show_correlation(metrics_patch)

# class level
metrics_class <- calculate_lsm(landscape, what = 'class',
                               verbose = FALSE)

metrics_plot_class <- show_correlation(metrics_class)

# patch and class level
metrics_mult <- calculate_lsm(landscape,
                              what = c("patch", "class"),
                              verbose = FALSE)

metrics_plot_mult <- show_correlation(metrics_mult)

# landscape level
metrics_land <- calculate_lsm(landscape_stack,
                              level = "landscape",
                              type = "area and edge metric",
                              verbose = FALSE)

metrics_plot_land <- show_correlation(metrics_land)

# correlation tibble
correlations <- calculate_correlation(metrics_mult)

metrics_plot_class_corr <- show_correlation(correlations)

test_that("show_correlation returns a plot on patch level", {

    expect_true(inherits(x = metrics_plot_patch, what = c("gg","ggplot")))
})

test_that("show_correlation returns a plot on class level", {

    expect_true(inherits(x = metrics_plot_class, what = c("gg","ggplot")))
})

test_that("show_correlation returns a plot for patch and class level", {

    expect_true(inherits(x = metrics_plot_mult, what = c("gg","ggplot")))
})

test_that("show_correlation returns a plot for landscape level", {

    expect_true(inherits(x = metrics_plot_land, what = c("gg","ggplot")))
})

test_that("show_correlation returns a plot for correlation list", {

    expect_true(inherits(x = metrics_plot_class_corr, what = c("gg","ggplot")))
})

context("calculate_correlation")


test_that("calculate_correlation returns a list if two levels are present", {

    metrics <- calculate_lsm(landscape_stack,
                             level = c("patch", "class"),
                             type = "area and edge metric")

    result <- calculate_correlation(metrics, method = "pearson")

    expect_length(result, n = 2)
    expect_true(all(names(result) == c("patch", "class")))
})

test_that("calculate_correlation works on landscape level for RasterStacks", {

    metrics <- calculate_lsm(landscape_stack,
                             level = "landscape",
                             type = "diversity metric",
                             classes_max = 3)

    result <- calculate_correlation(metrics, method = "pearson", simplify = TRUE)

    expect_is(result, "data.frame")
})

test_that("calculate_correlation returns warnings", {

    metrics <- calculate_lsm(landscape_stack,
                             level = c("patch", "class"),
                             type = "area and edge metric")

    expect_warning(calculate_correlation(metrics, method = "pearson", simplify = TRUE),
                   regexp = "Simplifying only possible if one level is present.",
                   fixed = FALSE)
})

test_that("calculate_correlation returns error of only one metric is present", {

    metrics <- calculate_lsm(landscape,
                             what = "lsm_p_area")

    expect_error(calculate_correlation(metrics, method = "pearson"),
                 regexp = "Please provide input with more than one metric.",
                 fixed = FALSE)

    metrics <- calculate_lsm(landscape,
                             what = "lsm_c_ca")

    expect_error(calculate_correlation(metrics, method = "pearson"),
                 regexp = "Please provide input with more than one metric.",
                 fixed = FALSE)
})

test_that("calculate_correlation returns error of only one layer is present on landscape level", {

    metrics <- calculate_lsm(landscape,
                             level = "landscape",
                             type = "area and edge metric")

    expect_error(calculate_correlation(metrics, method = "pearson"),
                 regexp = "Correlation on landscape level only possible for several landscapes.",
                 fixed = FALSE)
})

context("test-list_lsm")

test_that("all argument works for list_lsm()", {

    result_level <- list_lsm(level = "landscape")
    result_metric <- list_lsm(metric = "iji")
    result_name <- list_lsm(name = "patch area")
    result_type <- list_lsm(type = "aggregation metric")

    expect_true(all(result_level$level == "landscape"))
    expect_true(all(result_metric$metric == "iji"))
    expect_true(all(result_name$name == "patch area"))
    expect_true(all(result_type$type == "aggregation metric"))

    expect_is(result_level, "data.frame")
    expect_is(result_metric, "data.frame")
    expect_is(result_name, "data.frame")
    expect_is(result_type, "data.frame")
})

test_that("what argument still works for list_lsm()", {

    result <- list_lsm(what = c("patch", "lsm_l_ta"))

    expect_true(all(result$level %in% c("patch", "landscape")))
    expect_true(result$metric[result$level == "landscape"] == "ta")

    expect_is(result, "data.frame")
})

test_that("simplify returns vector", {

    result <- list_lsm(simplify = TRUE)

    expect_is(result, "character")
})

test_that("returns warning if what and other argument is specifiedr", {

    expect_warning(list_lsm(level = "landscape", what = "class"),
                   regexp = "only using 'what' argument")

    result <- list_lsm(level = "landscape", what = "class", verbose = FALSE)
    expect_true(all(result$level == "class"))
})


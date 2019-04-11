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

test_that("returns warning if what and other argument is specified", {

    expect_warning(list_lsm(level = "landscape", what = "class"),
                   grep = "Only using 'what' argument.",
                   fixed = TRUE)

    result <- list_lsm(level = "landscape", what = "class", verbose = FALSE)

    expect_true(all(result$level == "class"))
})

test_that("Negative subset works for list_lsm()", {

    result_level <- list_lsm(level = "-landscape")
    result_metric <- list_lsm(metric = "-iji")
    result_name <- list_lsm(name = "-patch area")
    result_type <- list_lsm(type = "-aggregation metric")

    result_mixed <- list_lsm(level = "class",
                             type = "-aggregation metric",
                             metric = "-cai")

    expect_false(any(result_level$level == "landscape"))
    expect_false(any(result_metric$metric == "iji"))
    expect_false(any(result_name$name == "patch area"))
    expect_false(any(result_type$type == "aggregation metric"))

    expect_false(all(any(result_mixed$level != "class"),
                     any(result_mixed$type == "aggregation metric"),
                     any(result_mixed$metric == "cai")))


    expect_is(result_level, "data.frame")
    expect_is(result_metric, "data.frame")
    expect_is(result_name, "data.frame")
    expect_is(result_type, "data.frame")
    expect_is(result_mixed, "data.frame")
})

test_that("list_lsm returns error", {

    expect_error(list_lsm(what = "-patch"),
                 grep = "Negative strings not allowed for 'what' argument.
                 Please use other arguments for negative subsets.",
                 fixed = TRUE)

    expect_error(list_lsm(level = c("-patch", "landscape")),
                 grep = "Mixing of positive and negative strings as subset not
                 allowed for the same argument.",
                 fixed = TRUE)

    expect_error(list_lsm(what = "lsm_p_made_up"),
                 grep = "Selected metrics do not exist. Please use 'list_lsm()'
                 to see all available metrics.",
                 fixed = TRUE)

})

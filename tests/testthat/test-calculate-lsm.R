test_that("calculate_lsm can take different raster inputs", {

    expect_s3_class(calculate_lsm(landscape, what = "lsm_l_ta",
                            verbose = FALSE), "tbl_df")
    expect_s3_class(calculate_lsm(landscape_stack, what = "lsm_l_ta",
                            verbose = FALSE), "tbl_df")
    expect_s3_class(calculate_lsm(landscape_list, what = "lsm_l_ta",
                            verbose = FALSE), "tbl_df")

    expect_s3_class(calculate_lsm(landscape_ras, what = "lsm_l_ta",
                            verbose = FALSE), "tbl_df")

    expect_s3_class(calculate_lsm(augusta_nlcd_stars, what = "lsm_l_ta",
                            verbose = FALSE), "tbl_df")

})

test_that("calculate_lsm can calculate patch metrics", {

    patch_metrics <- calculate_lsm(landscape, what = "patch",
                                   verbose = FALSE)

    expect_s3_class(patch_metrics, "tbl_df")
    expect_true(all(patch_metrics$level == "patch"))
    expect_true(ncol(patch_metrics) > 0)
})

test_that("calculate_lsm can calculate class metrics", {

    class_metrics <- calculate_lsm(landscape, what = "class",
                                   verbose = FALSE)

    expect_s3_class(class_metrics, "tbl_df")
    expect_true(all(class_metrics$level == "class"))
    expect_true(ncol(class_metrics) > 0)
})

test_that("calculate_lsm can calculate landscape metrics", {

    landscape_metrics <- calculate_lsm(landscape, what = "landscape",
                                       verbose = FALSE)

    expect_s3_class(landscape_metrics, "tbl_df")
    expect_true(all(landscape_metrics$level == "landscape"))
    expect_true(ncol(landscape_metrics) > 0)
})

test_that("calculate_lsm can take all metrics", {

    all_metrics <- calculate_lsm(landscape,
                                 verbose = FALSE)

    expect_true(all(all_metrics$level %in% c("patch", "class","landscape")))
    expect_s3_class(all_metrics, "tbl_df")
    expect_true(ncol(all_metrics) > 0)
})

test_that("calculate_lsm can take specific metrics", {

    specific_metrics <- calculate_lsm(landscape,
                                      what = c("lsm_p_enn", "lsm_c_ed"),
                                      verbose = FALSE)

    expect_true(all(specific_metrics$metric %in% c("ed", "enn")))
    expect_s3_class(specific_metrics, "tbl_df")
    expect_true(ncol(specific_metrics) > 0)
})

test_that("calculate_lsm can take level argument", {

    specific_metrics_patch <- calculate_lsm(landscape,
                                            level = "patch",
                                            verbose = FALSE)

    specific_metrics_class <- calculate_lsm(landscape,
                                            level = "class",
                                            verbose = FALSE)

    specific_metrics_landscape <- calculate_lsm(landscape,
                                                level = "landscape",
                                                verbose = FALSE)

    expect_true(all(specific_metrics_patch$level == "patch"))
    expect_true(all(specific_metrics_class$level == "class"))
    expect_true(all(specific_metrics_landscape$level == "landscape"))
})


test_that("calculate_lsm can take metric argument", {

    specific_metrics_area <- calculate_lsm(landscape,
                                           metric = "area",
                                           verbose = FALSE)

    metrics <- vapply(strsplit(specific_metrics_area$metric,
                               split = "_"), FUN = function(x) x[1],
                      FUN.VALUE = character(1))

    expect_true(all(metrics == "area"))
})

test_that("calculate_lsm can take name argument", {

    specific_metrics_name <- calculate_lsm(landscape, name = "core area",
                                           verbose = FALSE)

    metrics <- vapply(strsplit(specific_metrics_name$metric,
                               split = "_"), FUN = function(x) x[1],
                      FUN.VALUE = character(1))

    expect_true(all(metrics == "core"))
})

test_that("calculate_lsm can take type argument", {

    specific_metrics_type <- calculate_lsm(landscape,
                                           type = "aggregation metric",
                                           full_name = TRUE,
                                           verbose = FALSE)

    expect_true(all(specific_metrics_type$type == "aggregation metric"))
})

test_that("NA is returned if no cell has a value", {

    all_metric <- list_lsm()

    result <- calculate_lsm(landscape_NA,
                            verbose = FALSE)

    expect_equal(object = nrow(result), expected = nrow(all_metric))
    expect_true(object = all(result$metric %in% all_metric$metric))
    expect_true(object = all(is.na(result$value)))
})

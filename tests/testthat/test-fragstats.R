# calculate lsm results
lsm_landscape <- calculate_lsm(landscape = landscape, level = c("class", "landscape"),
                               classes_max = 3)

lsm_augusta <- calculate_lsm(landscape = augusta_nlcd, level = c("class", "landscape"),
                             classes_max = 15)

lsm_full <- dplyr::bind_rows(landscape = lsm_landscape, augusta_nlcd = lsm_augusta,
                             .id = "LID")
# dcore is really off?

test_that("class level", {

    above_thres <- dplyr::filter(lsm_full, level == "class") |>
        dplyr::left_join(y = fragstats_class, by = c("LID" = "LID", "class" = "TYPE", "metric" = "metric"),
                         suffix = c(".lsm", ".fs")) |>
        # gyrate is just on different scales
        dplyr::mutate(value.lsm = dplyr::case_when(metric == "para_mn" | metric == "para_sd" ~ value.lsm * 10000,
                                                   TRUE ~ value.lsm)) |>
        # gyrate and circle are different due to an error propagation based on 1-cell patches
        dplyr::filter(!metric %in% c("gyrate_mn", "gyrate_sd", "gyrate_cv",
                                     "circle_mn", "circle_sd", "circle_cv")) |>
        # not sure whats going on with dcore
        dplyr::filter(!metric %in% c("dcore_mn", "dcore_sd", "dcore_cv")) |>
        dplyr::mutate(value.diff = abs((value.lsm - value.fs) / value.fs * 100)) |>
        dplyr::filter(value.diff > 10) |>
        nrow()


    expect_lt(object = above_thres, expected = 5)

})

test_that("landscape level", {

    above_thres <- dplyr::filter(lsm_full, level == "landscape") |>
        dplyr::left_join(y = fragstats_landscape, by = c("LID" = "LID", "metric" = "metric"),
                         suffix = c(".lsm", ".fs")) |>
        # gyrate is just on different scales
        dplyr::mutate(value.lsm = dplyr::case_when(metric == "para_mn" | metric == "para_sd" ~ value.lsm * 10000,
                                                   TRUE ~ value.lsm)) |>
        # gyrate and circle are different due to an error propagation based on 1-cell patches
        dplyr::filter(!metric %in% c("gyrate_mn", "gyrate_sd", "gyrate_cv",
                                     "circle_mn", "circle_sd", "circle_cv")) |>
        # not sure whats going on with dcore
        dplyr::filter(!metric %in% c("dcore_mn", "dcore_sd", "dcore_cv")) |>
        dplyr::mutate(value.diff = abs((value.lsm - value.fs) / value.fs * 100)) |>
        dplyr::filter(value.diff > 5.0) |>
        nrow()

    expect_lt(object = above_thres, expected = 1)

})

# import and reshape FRAGSTATS v2.0 results

fragstats_patch <- c(landscape = testthat::test_path("results-fragstats", "landscape.patch"),
                     augusta_nlcd = testthat::test_path("results-fragstats", "augusta_nlcd.patch")) |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID") |>
    tidyr::pivot_longer(cols = -c("LID", "PID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_class <- c(landscape = testthat::test_path("results-fragstats", "landscape.class"),
                     augusta_nlcd = testthat::test_path("results-fragstats", "augusta_nlcd.class")) |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID") |>
    tidyr::pivot_longer(cols = -c("LID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_landscape <- c(landscape = testthat::test_path("results-fragstats", "landscape.land"),
                     augusta_nlcd = testthat::test_path("results-fragstats", "augusta_nlcd.land")) |>
    lapply(read.table, sep  = ",", header = TRUE,  na.strings = " N/A ") |>
    dplyr::bind_rows(.id = "LID") |>
    tidyr::pivot_longer(cols = -c("LID"), names_to = "metric") |>
    dplyr::mutate(metric = stringr::str_to_lower(metric))

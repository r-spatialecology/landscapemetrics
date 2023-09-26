# import and reshape FRAGSTATS v2.0 results

fragstats_patch <- landscapemetrics:::internal_data$fs_data$patch |>
    tidyr::pivot_longer(cols = -c("LID", "PID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_class <- landscapemetrics:::internal_data$fs_data$class |>
    tidyr::pivot_longer(cols = -c("LID", "TYPE"), names_to = "metric") |>
    dplyr::mutate(TYPE = stringr::str_remove_all(TYPE, pattern = " "),
                  TYPE = as.integer(stringr::str_remove(TYPE, pattern = "cls_")),
                  metric = stringr::str_to_lower(metric))

fragstats_landscape <- landscapemetrics:::internal_data$fs_data$landscape |>
    tidyr::pivot_longer(cols = -c("LID"), names_to = "metric") |>
    dplyr::mutate(metric = stringr::str_to_lower(metric))

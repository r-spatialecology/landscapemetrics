lsm_landscape_output <- function(metric, value) {
    tibble::new_tibble(list(
        level = rep("landscape", length(value)),
        class = rep(as.integer(NA), length(value)),
        id = rep(as.integer(NA), length(value)),
        metric = rep(metric, length(value)),
        value = as.double(value)
    ))
}

lsm_class_output <- function(metric, class, value) {
    tibble::new_tibble(list(
        level = rep("class", length(value)),
        class = as.integer(class),
        id = rep(as.integer(NA), length(value)),
        metric = rep(metric, length(value)),
        value = as.double(value)
    ))
}

lsm_patch_output <- function(metric, class, value, id = NULL) {
    if (is.null(id)) {
        id <- seq_along(value)
    }

    tibble::new_tibble(list(
        level = rep("patch", length(value)),
        class = as.integer(class),
        id = as.integer(id),
        metric = rep(metric, length(value)),
        value = as.double(value)
    ))
}

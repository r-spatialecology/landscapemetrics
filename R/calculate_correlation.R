#' Calculate correlation
#'
#' @description Calculate correlation
#'
#' @param metrics Tibble with results of as returned by the landscapemetrics package.
#' @param method Type of correlation. See \code{link{cor}} for details.
#' @param diag If FALSE, values on the diagonal will be NA.
#' @param simplify If TRUE and only one level is present, only a tibble is returned.
#'
#' @details The functions calculates the correlation between all metrics. In order to calculate correlations,
#' for the landscape level more than one landscape needs to be present. All input
#' must be structured as returned by the **landscapemetrics** package.
#'
#' @return list
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' metrics <- calculate_lsm(landscape, what = c("patch", "class"))
#' calculate_correlation(metrics, method = "pearson")
#'
#' @export
calculate_correlation <- function(metrics, method = "pearson",
                                  diag = TRUE, simplify = FALSE) {

    # get all present levels
    present_levels <- unique(metrics$level)

    # patch level present
    if ("patch" %in% present_levels) {

        # which rows are patch level
        patch_index <- which(metrics$level == "patch")

        # get rows with patch level
        metrics_patch <- metrics[patch_index, ]

        # check of more than one metric is present
        if (length(unique(metrics_patch$metric)) == 1) {
            stop("Please provide input with more than one metric.", call. = FALSE)
        }

        # bring to long format
        metrics_patch_wide <- stats::xtabs(value ~ id + metric,
                                           data = metrics_patch[, c(4:6)])

        # get correlation
        correlation_matrix_patch <- stats::cor(metrics_patch_wide,
                                               method = method)

        # only upper triangle and diag according to seeting
        correlation_matrix_patch[upper.tri(correlation_matrix_patch,
                                           diag = !diag)] <- NA

        # reformat to data frame
        correlation_matrix_patch_df <- data.frame(
            metric_1 = rownames(correlation_matrix_patch)[row(correlation_matrix_patch)],
            metric_2 = colnames(correlation_matrix_patch)[col(correlation_matrix_patch)],
            value = c(correlation_matrix_patch))

        # only complete cases
        patch <- tibble::as_tibble(correlation_matrix_patch_df[stats::complete.cases(correlation_matrix_patch_df), ])
    }

    # class level metrics
    if ("class" %in% present_levels) {

        # which rows are class level
        class_index <- which(metrics$level == "class")

        # get rows
        metrics_class <- metrics[class_index, ]

        # check if more than one metric is present
        if (length(unique(metrics_class$metric)) == 1) {
            stop("Please provide input with more than one metric.", call. = FALSE)
        }

        # reshape to long format
        metrics_class_wide <- stats::xtabs(value ~ class + metric,
                                           data = metrics_class[, c(3, 5:6)])

        # get correlation
        correlation_matrix_class <- stats::cor(metrics_class_wide,
                                               method = method)

        # only lower triangle and diag according to setting
        correlation_matrix_class[upper.tri(correlation_matrix_class, diag = !diag)] <- NA

        # reshape to long format
        correlation_matrix_class_df <- data.frame(
            metric_1 = rownames(correlation_matrix_class)[row(correlation_matrix_class)],
            metric_2 = colnames(correlation_matrix_class)[col(correlation_matrix_class)],
            value = c(correlation_matrix_class))

        # only complete cases
        class <- tibble::as_tibble(correlation_matrix_class_df[stats::complete.cases(correlation_matrix_class_df), ])
    }

    # landscape
    if ("landscape" %in% present_levels) {

        # which rows are landscape level
        landscape_index <- which(metrics$level == "landscape")

        # get rows
        metrics_landscape <- metrics[landscape_index, ]

        # check if more than one layer is present
        if (length(unique(metrics_landscape$layer)) == 1) {
            stop("Correlation on landscape level only possible for several landscapes.", call. = FALSE)
        } else {
            if (length(unique(metrics_landscape$metric)) == 1) {
                stop("Please provide input with more than one metric.", call. = FALSE)
            }

            # reshape to wide format
            metrics_landscape_wide <- stats::xtabs(value ~ layer + metric,
                                                   data = metrics_landscape[, c(1, 5:6)])

            # get correlation
            correlation_matrix_landscape <- stats::cor(metrics_landscape_wide, method = method)

            # only upper triangle and diag according to setting
            correlation_matrix_landscape[upper.tri(correlation_matrix_landscape, diag = !diag)] <- NA

            # save in data frame
            correlation_matrix_landscape_df <- data.frame(
                metric_1 = rownames(correlation_matrix_landscape)[row(correlation_matrix_landscape)],
                metric_2 = colnames(correlation_matrix_landscape)[col(correlation_matrix_landscape)],
                value = c(correlation_matrix_landscape))

            # only complete cases
            landscape <- tibble::as_tibble(correlation_matrix_landscape_df[stats::complete.cases(correlation_matrix_landscape_df), ])
        }
    }

    # get all present levels
    correlation_list <- mget(x = c("patch", "class", "landscape"),
                             ifnotfound = list(NA))

    # remove non-present levels
    correlation_list <- correlation_list[!is.na(correlation_list)]

    # only return tibble if possible
    if (simplify) {

        if (length(correlation_list) == 1) {
            correlation_list <- correlation_list[[1]]
        } else{
            warning("Simplifying only possible if one level is present.", call. = FALSE)
        }
    }

    return(correlation_list)
}

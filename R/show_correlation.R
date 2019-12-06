#' Show correlation
#'
#' @description Show correlation
#'
#' @param data Tibble with results of as returned by the landscapemetrics package.
#' @param method Type of correlation. See \code{link{cor}} for details.
#' @param diag If FALSE, values on the diagonal will be NA and not plotted.
#' @param labels If TRUE, the correlation value will be added as text.
#' @param vjust Will be passed on to ggplot2 as vertical justification of x-axis text.
#' @param text_size Text size of the plot.
#'
#' @details The functions calculates the correlation between all metrics. In order to calculate correlations,
#' for the landscape level more than one landscape needs to be present. All input
#' must be structured as returned by the **landscapemetrics** package.
#'
#' @return ggplot
#'
#' @examples
#' metrics <- calculate_lsm(landscape, what = c("patch", "class"))
#' show_correlation(data = metrics, method = "pearson")
#'
#' \dontrun{
#' metrics <- calculate_lsm(landscape, what = c("patch", "class"))#'
#' correlations <- calculate_correlation(metrics)
#' show_correlation(data = correlations, method = "pearson")
#'
#' }
#'
#' @aliases show_correlation
#' @rdname show_correlation
#'
#' @export
show_correlation <- function(data, method = "pearson",
                             diag = TRUE, labels = FALSE,
                             vjust = 0, text_size = 15) {

    # metrics tibble
    if (all(names(data) == c("layer", "level", "class", "id", "metric", "value"))) {

        present_levels <- unique(data$level)

        if ("patch" %in% present_levels) {

            patch_index <- which(data$level == "patch")

            metrics_patch <- data[patch_index, ]

            if (length(unique(metrics_patch$metric)) == 1) {
                stop("Please provide input with more than one metric")
            }

            metrics_patch_wide <- stats::xtabs(value ~ id + metric,
                                               data = metrics_patch[, c(4:6)])

            attr(metrics_patch_wide, "class") <- NULL

            attr(metrics_patch_wide, "call") <- NULL

            correlation_matrix_patch <- stats::cor(metrics_patch_wide,
                                                   method = method)

            correlation_matrix_patch[upper.tri(correlation_matrix_patch,
                                               diag = !diag)] <- NA

            correlation_matrix_patch_df <-
                data.frame(
                    metric_1 = rownames(correlation_matrix_patch)[row(correlation_matrix_patch)],
                    metric_2 = colnames(correlation_matrix_patch)[col(correlation_matrix_patch)],
                    value = c(correlation_matrix_patch)
                )

            patch <-
                correlation_matrix_patch_df[stats::complete.cases(correlation_matrix_patch_df),]

        }

        if ("class" %in% present_levels) {

            class_index <- which(data$level == "class")

            metrics_class <- data[class_index, ]

            if (length(unique(metrics_class$metric)) == 1) {
                stop("Please provide input with more than one metric")
            }

            metrics_class_wide <- stats::xtabs(value ~ class + metric,
                                               data = metrics_class[, c(3, 5:6)])

            attr(metrics_class_wide, "class") <- NULL

            attr(metrics_class_wide, "call") <- NULL

            correlation_matrix_class <- stats::cor(metrics_class_wide,
                                                   method = method)

            correlation_matrix_class[upper.tri(correlation_matrix_class,
                                               diag = !diag)] <- NA

            correlation_matrix_class_df <-
                data.frame(
                    metric_1 = rownames(correlation_matrix_class)[row(correlation_matrix_class)],
                    metric_2 = colnames(correlation_matrix_class)[col(correlation_matrix_class)],
                    value = c(correlation_matrix_class)
                )

            class <-
                correlation_matrix_class_df[stats::complete.cases(correlation_matrix_class_df),]
        }

        if ("landscape" %in% present_levels) {

            landscape_index <- which(data$level == "landscape")

            metrics_landscape <- data[landscape_index, ]

            if (length(unique(metrics_landscape$layer)) == 1) {
                stop("Correlation on landscape level only possible for several landscapes")
            } else {
                if (length(unique(metrics_landscape$metric)) == 1) {
                    stop("Please provide input with more than one metric")
                }

                metrics_landscape_wide <- stats::xtabs(value ~ layer + metric,
                                                       data = metrics_landscape[, c(1, 5:6)])

                attr(metrics_landscape_wide, "landscape") <- NULL

                attr(metrics_landscape_wide, "call") <- NULL

                correlation_matrix_landscape <- stats::cor(metrics_landscape_wide, method = method)

                correlation_matrix_landscape[upper.tri(correlation_matrix_landscape,
                                                       diag = !diag)] <- NA

                correlation_matrix_landscape_df <-
                    data.frame(
                        metric_1 = rownames(correlation_matrix_landscape)[row(correlation_matrix_landscape)],
                        metric_2 = colnames(correlation_matrix_landscape)[col(correlation_matrix_landscape)],
                        value = c(correlation_matrix_landscape)
                    )

                landscape <-
                    correlation_matrix_landscape_df[stats::complete.cases(correlation_matrix_landscape_df),]

            }
        }

        plot_list <- mget(x = c("patch", "class", "landscape"),
                          ifnotfound = list(NA))
    }

    # tibble with correlations
    else if (inherits(x = data, what = "list")) {

        # check if tibble contains correlations
        if (!all(vapply(data, FUN = function(x) all(names(x) == c("metric_1", 'metric_2', "value")),
               FUN.VALUE = logical(1)))) {

            stop("Please provide list with correlations calculated using 'calculate_correlation()'.", call. = FALSE)

        }

        # save data as plot_list
        plot_list <- data

        # get present levels
        present_levels <- names(data)

        # vector to check if all levels are present
        possible_levels <- c("patch", "class", "landscape")

        # which levels are missing
        missing_levels <- possible_levels[!possible_levels %in% present_levels]

        # add NA for missing levels
        plot_list[missing_levels] <- NA

    }

    else {
        stop("Please provide either tibble with landscape metrics or list with correlations.", call. = FALSE)
    }

    if (length(present_levels) == 1) {

        plot_corrs <- ggplot2::ggplot(data = plot_list[[present_levels]],
                                      ggplot2::aes(
                                          x = metric_1,
                                          y = metric_2,
                                          fill = value
                                      )) +
            ggplot2::geom_tile() +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red",
                mid = "white",
                midpoint = 0,
                limit = c(-1, 1),
                name = paste0("Correlation\n(Method: ",
                              method, ")")
            ) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "",
                          y = "",
                          title = paste(toupper(substr(present_levels, 1, 1)),
                                        substr(present_levels, 2, nchar(present_levels))
                                        , " Level", sep="")) +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(
                    angle = 90, vjust = vjust
                ),
                text = ggplot2::element_text(size = text_size)
            ) +
            ggplot2::coord_fixed()

        if(labels) {
            plot_corrs <- plot_corrs +
                ggplot2::geom_text(data = plot_list[[present_levels]],
                                   ggplot2::aes(
                                       x = metric_1,
                                       y = metric_2,
                                       label = round(value, 2)
                                   ))
        }

    } else {
        if (all(!is.na(plot_list$patch))) {
            patch_corrs <- plot_list$patch
            patch_corrs$id <- "Patch Level"
        }

        if (all(!is.na(plot_list$class))) {
            class_corrs <- plot_list$class
            class_corrs$id <- "Class Level"
        }

        if (all(!is.na(plot_list$landscape))) {
            landscape_corrs <- plot_list$landscape
            landscape_corrs$id <- "Landscape Level"
        }

        corrs_list <-
            mget(
                x = c("patch_corrs", "class_corrs", "landscape_corrs"),
                ifnotfound = list(NA)
            )


        corrs_df <- do.call(rbind, corrs_list)
        corrs_df <- corrs_df[!is.na(corrs_df$value),]

        corrs_df$id <-
            factor(corrs_df$id, levels = unique(corrs_df$id))

        plot_corrs <- ggplot2::ggplot(data = corrs_df,
                                      ggplot2::aes(
                                          x = metric_1,
                                          y = metric_2,
                                          fill = value
                                      )) +
            ggplot2::facet_wrap(id ~ ., scales = "free") +
            ggplot2::geom_tile() +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_gradient2(
                low = "blue",
                high = "red",
                mid = "white",
                midpoint = 0,
                limit = c(-1, 1),
                name = paste0("Correlation\n(Method: ",
                              method, ")")
            ) +
            ggplot2::theme_minimal() +
            ggplot2::labs(x = "", y = "") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = vjust),
                           text = ggplot2::element_text(size = text_size)
            )

        if(labels) {
            plot_corrs <- plot_corrs +
                ggplot2::geom_text(data = corrs_df,
                                   ggplot2::aes(
                                       x = metric_1,
                                       y = metric_2,
                                       label = round(value, 2)
                                   ))
        }

    }

    suppressWarnings(return(plot_corrs))
}

#' Show correlation
#'
#' @description Show correlation
#'
#' @param metrics tibble with results of as returned by the landscapemetrics package
#' @param method type of correlation. See \code{link{cor}} for details
#' @param text_size Text size of the plot
#'
#' @details The functions calculates the correlation between all metrics. In order to calculate correlations,
#' for the landscape level more than one landscape needs to be present. All input
#' must be structured as returned by the **landscapemetrics** package.
#'
#' @return ggplot
#'
#' @examples
#' metrics <- calculate_lsm(landscape, what = c("patch", "class"))
#' show_correlation(metrics, method = "pearson")
#'
#' @aliases show_correlation
#' @rdname show_correlation
#'
#' @export
show_correlation <-
    function(metrics,
             method = "pearson",
             text_size = 15) {

        present_levels <- unique(metrics$level)

        if ("patch" %in% present_levels) {
            metrics_patch <- dplyr::filter(metrics, level == "patch")

            if (length(unique(metrics_patch$metric)) == 1) {
                stop("Please provide input with more than one metric")
            }

            metrics_patch_wide <- stats::xtabs(value ~ id + metric,
                                               data = metrics_patch[, c(4:6)])
            attr(metrics_patch_wide, "class") <- NULL
            attr(metrics_patch_wide, "call") <- NULL

            correlation_matrix_patch <-
                stats::cor(metrics_patch_wide[, 2:ncol(metrics_patch_wide)],
                           method = method)

            correlation_matrix_patch[upper.tri(correlation_matrix_patch,
                                               diag = TRUE)] <- NA

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
            metrics_class <- dplyr::filter(metrics, level == "class")

            if (length(unique(metrics_class$metric)) == 1) {
                stop("Please provide input with more than one metric")
            }

            metrics_class_wide <-
                stats::xtabs(value ~ class + metric,
                             data = metrics_class[, c(3, 5:6)])
            attr(metrics_class_wide, "class") <- NULL
            attr(metrics_class_wide, "call") <- NULL

            correlation_matrix_class <-
                stats::cor(metrics_class_wide[, 2:ncol(metrics_class_wide)],
                           method = method)

            correlation_matrix_class[upper.tri(correlation_matrix_class,
                                               diag = TRUE)] <- NA

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
            metrics_landscape <- dplyr::filter(metrics, level == "landscape")

            if (length(unique(metrics_landscape$layer)) == 1) {
                stop("Correlation on landscape level only possible for several landscapes")
            } else{
                if (length(unique(metrics_landscape$metric)) == 1) {
                    stop("Please provide input with more than one metric")
                }

                metrics_landscape_wide <-
                    stats::xtabs(value ~ layer + metric,
                                 data = metrics_landscape[, c(1, 5:6)])
                attr(metrics_landscape_wide, "landscape") <- NULL
                attr(metrics_landscape_wide, "call") <- NULL

                correlation_matrix_landscape <-
                    stats::cor(metrics_landscape_wide[, 2:ncol(metrics_landscape_wide)],
                               method = method)

                correlation_matrix_landscape[upper.tri(correlation_matrix_landscape,
                                                       diag = TRUE)] <-
                    NA

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

        plot_list <-
            mget(
                x = c(
                    "patch",
                    "class",
                    "landscape"
                ),
                ifnotfound = list(NA)
            )


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
                        angle = 45,
                        vjust = 1,
                        size = 12,
                        hjust = 1
                    ),
                    text = ggplot2::element_text(size = text_size)
                ) +
                ggplot2::coord_fixed()

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

            corrs_df <- dplyr::filter(
                dplyr::group_by(
                    do.call(rbind, corrs_list),
                    id),
                !all(is.na(value))
                )

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
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45,
                        vjust = 1,
                        size = 12,
                        hjust = 1
                    ),
                    text = ggplot2::element_text(size = text_size)
                )

        }

        return(plot_corrs)

}

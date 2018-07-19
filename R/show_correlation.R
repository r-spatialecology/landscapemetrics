#' Show correlation
#'
#' @description Show correlation
#'
#' @param metrics tibble with results of as returned by the landscapemetrics package
#' @param level level of the metrics (`patch`, `class`, `landscape`)
#' @param method type of correlation. See \code{link{cor}} for details
#' @param text_size Text size of the plot

#' @details The functions calculates the correlation between all metrics. All metrics
#' of the input tibble must be from the same level. In order to calculate correlations,
#' for the landscape level more than one landscape needs to be present. All input
#' must be structured as returned by the landscapemetrics package.
#'
#' @return ggplot
#'
#' @examples
#' results <- lsm_calculate(landscape, what = "patch")
#' show_correlation(results, level = "patch", method = "pearson")
#'
#' @aliases show_correlation
#' @rdname show_correlation
#'
#' @export
show_correlation <- function(metrics, level, method = "pearson", text_size = 15) {

    if(length(unique(metrics$metric)) == 1){
        stop("Please provide input with more than one metric")
        }

    if(level == "patch") {

        metrics_wide <- tidyr::spread(data = metrics[, c(4:6)],
                                      key = metric,
                                      value = value)

        correlation_matrix <- stats::cor(metrics_wide[2:ncol(metrics_wide)],
                                         method = method)

        correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA

        correlation_matrix_df <-
            data.frame(
                metric_1 = rownames(correlation_matrix)[row(correlation_matrix)],
                metric_2 = colnames(correlation_matrix)[col(correlation_matrix)],
                value = c(correlation_matrix)
            )

        correlation_matrix_df <- correlation_matrix_df[stats::complete.cases(correlation_matrix_df),]
    }

    else if(level == "class") {

        metrics_wide <- tidyr::spread(data = metrics[, c(3, 5:6)],
                                      key = metric,
                                      value = value)

        correlation_matrix <- stats::cor(metrics_wide[2:ncol(metrics_wide)],
                                         method = method)

        correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA

        correlation_matrix_df <-
            data.frame(
                metric_1 = rownames(correlation_matrix)[row(correlation_matrix)],
                metric_2 = colnames(correlation_matrix)[col(correlation_matrix)],
                value = c(correlation_matrix)
            )

        correlation_matrix_df <- correlation_matrix_df[stats::complete.cases(correlation_matrix_df),]
    }

    else if(level == "landscape") {

        if(length(unique(metrics$layer)) == 1) {
            stop("Correlation on landscape level only possible for several landscapes")
        }

        else{
            metrics_wide <- tidyr::spread(data = metrics[, c(1, 5:6)],
                                          key = metric,
                                          value = value)

            correlation_matrix <- stats::cor(metrics_wide[2:ncol(metrics_wide)],
                                             method = method)

            correlation_matrix[upper.tri(correlation_matrix, diag = TRUE)] <- NA

            correlation_matrix_df <-
                data.frame(
                    metric_1 = rownames(correlation_matrix)[row(correlation_matrix)],
                    metric_2 = colnames(correlation_matrix)[col(correlation_matrix)],
                    value = c(correlation_matrix)
                )

            correlation_matrix_df <- correlation_matrix_df[stats::complete.cases(correlation_matrix_df),]

        }

    }

    else{stop("Please select 'patch', 'class' or 'landscape' as level")}

    plot <- ggplot2::ggplot(data = correlation_matrix_df,
                           ggplot2::aes(x = metric_1, y = metric_2, fill = value)) +
        ggplot2::geom_tile() +
        ggplot2::geom_tile(color = "white")+
        ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                     midpoint = 0, limit = c(-1,1),
                                     name=paste0("Correlation\n(Method: ", method, ")")) +
        ggplot2::theme_minimal()+
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1,
                                                 size = 12, hjust = 1),
                      text = ggplot2::element_text(size = text_size)) +
        ggplot2::coord_fixed()

    return(plot)

}

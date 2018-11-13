#' List landscape metrics
#'
#' @description List landscape metrics
#'
#' @param level Level of metrics. Either 'all', 'patch', 'class' or 'landscape' (or vector with combination).
#' @param metric Abbreviation of metrics.
#' @param full_name Full name of metrics.
#' @param type Type according to FRAGSTATS grouping.
#' @param simplify If true, function names are returned as vector.
#'
#' @details List all available landscape metrics depending on the provided filter arguments.
#' If an argument is not provided, automatically all possibilites are selected.
#' Furthermore, it is possible to only get a vector with all function names instead of the full tibble.
#'
#' @return tibble
#'
#' @examples
#' list_lsm(level = c("patch", "landscape"), type = "aggregation metric")
#' list_lsm(metric = "area", simplify = TRUE)
#'
#' @aliases list_lsm
#' @rdname list_lsm
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
list_lsm <- function(level = "all",
                     metric = NULL,
                     full_name = NULL,
                     type = NULL,
                     simplify = FALSE) {

    lsm_abbreviations_names_modified <- landscapemetrics::lsm_abbreviations_names

    lsm_abbreviations_names_modified$metric_new <- sapply(strsplit(lsm_abbreviations_names_modified$metric,
                                                                   split = "_"),
                                                          function(x) x[1])

    if(any(level %in% "all")) {
        level <- c("patch", "class", "landscape")
    }

    if(is.null(metric)){
       metric <- unique(lsm_abbreviations_names_modified$metric_new)
    }

    if(is.null(full_name)){
        full_name <- unique(lsm_abbreviations_names_modified$full_name)
    }

    if(is.null(type)){
        type <- unique(lsm_abbreviations_names_modified$type)
    }

    result <- dplyr::filter(lsm_abbreviations_names_modified,
                            level %in% !!level,
                            metric_new %in% !!metric,
                            full_name %in% !!full_name,
                            type %in% !!type)

    result <- dplyr::select(result, -metric_new)

    if(simplify) {
        result <- dplyr::pull(result, function_name)
    }

    return(result)
}


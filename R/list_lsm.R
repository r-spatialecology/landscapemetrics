#' List landscape metrics
#'
#' @description List landscape metrics
#'
#' @param level Level of metrics. Either 'patch', 'class' or 'landscape' (or vector with combination).
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param simplify If true, function names are returned as vector.
#' @param verbose Print warning messages

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
#' list_lsm(metric = "area", what = "lsm_p_shape")
#' list_lsm(metric = "area", what = c("patch", "lsm_l_ta"))
#' list_lsm(what = c("lsm_c_tca", "lsm_l_ta"))
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
list_lsm <- function(level = NULL,
                     metric = NULL,
                     name = NULL,
                     type = NULL,
                     what = NULL,
                     simplify = FALSE,
                     verbose = TRUE) {

    lsm_abbreviations_names_modified <- landscapemetrics::lsm_abbreviations_names

    lsm_abbreviations_names_modified$metric_new <- sapply(strsplit(lsm_abbreviations_names_modified$metric,
                                                                   split = "_"),
                                                          function(x) x[1])

    if(!is.null(what)) {

        if(!is.null(c(level, metric, name, type))) {

            level  <- NULL
            metric <- NULL
            name <- NULL
            type <- NULL

            if(verbose) {warning("only using 'what' argument", call. = FALSE)}
        }

        if(any(what %in% c("patch", "class", "landscape"))){
            level <- what[what %in% c("patch", "class", "landscape")]
            what <- what[!what %in% c("patch", "class", "landscape")]
        }

        result <- dplyr::filter(lsm_abbreviations_names_modified,
                                function_name %in% what | level %in% !!level)
    }

    else{

        if(is.null(level)) {
            level <- c("patch", "class", "landscape")
        }

        if(is.null(metric)){
            metric <- unique(lsm_abbreviations_names_modified$metric_new)
        }

        if(is.null(name)){
            name <- unique(lsm_abbreviations_names_modified$name)
        }

        if(is.null(type)){
            type <- unique(lsm_abbreviations_names_modified$type)
        }

        result <- dplyr::filter(lsm_abbreviations_names_modified,
                                level %in% !!level,
                                metric_new %in% !!metric,
                                name %in% !!name,
                                type %in% !!type)
    }

    result <- dplyr::select(result, -metric_new)

    if(simplify) {
        result <- result$function_name
    }

    return(result)
}


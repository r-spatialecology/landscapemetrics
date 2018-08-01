#' Calculate metrics
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "all", "patch", "class",
#' "landscape". The default is "all". It is also possible to specifiy functions
#' as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param count_boundary Include landscape boundary in edge length
#' @param classes_max Potential maximum number of present classes
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured. The default is "log2",
#' which compute entropy in "bits". "log" and "log10" can be also used.
#' @param full_name Should the full names of all functions be included in the
#' tibble.
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' calculate_metrics(landscape)
#' calculate_metrics(landscape, what = "patch")
#' }
#'
#' @aliases calculate_metrics
#'
#' @rdname calculate_metrics
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
calculate_metrics <- function(landscape,
                          what,
                          directions,
                          count_boundary,
                          classes_max,
                          neighbourhood,
                          ordered,
                          base,
                          full_name,
                          verbose) UseMethod("calculate_metrics")

#' @name calculate_metrics
#' @export
calculate_metrics.RasterLayer <- function(landscape,
                                      what = "all",
                                      directions = 8,
                                      count_boundary = FALSE,
                                      classes_max = NULL,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2",
                                      full_name = FALSE,
                                      verbose = TRUE) {

    calculate_metrics_internal(landscape,
                           what = what,
                           directions = directions,
                           count_boundary = count_boundary,
                           classes_max = classes_max,
                           neighbourhood = neighbourhood,
                           ordered = ordered,
                           base = base,
                           full_name = full_name,
                           verbose = verbose)

}

#' @name calculate_metrics
#' @export
calculate_metrics.RasterStack <- function(landscape,
                                      what = "all",
                                      directions = 8,
                                      count_boundary = FALSE,
                                      classes_max = NULL,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2",
                                      full_name = FALSE,
                                      verbose = TRUE) {

    purrr::map_dfr(raster::as.list(landscape),
                   calculate_metrics_internal,
                   what = what,
                   directions = directions,
                   count_boundary = count_boundary,
                   classes_max = classes_max,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   full_name = full_name,
                   verbose = verbose,
                   .id = "layer2") %>%
        dplyr::mutate(layer = as.integer(layer2)) %>%
        dplyr::select(-layer2)
}

#' @name calculate_metrics
#' @export
calculate_metrics.RasterBrick <- function(landscape,
                                      what = "all",
                                      directions = 8,
                                      count_boundary = FALSE,
                                      classes_max = NULL,
                                      neighbourhood = 4,
                                      ordered = TRUE,
                                      base = "log2",
                                      full_name = FALSE,
                                      verbose = TRUE) {

    purrr::map_dfr(raster::as.list(landscape),
                   calculate_metrics_internal,
                   what = what,
                   directions = directions,
                   count_boundary = count_boundary,
                   classes_max = classes_max,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   full_name = full_name,
                   verbose = verbose) %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name calculate_metrics
#' @export
calculate_metrics.list <- function(landscape,
                               what = "all",
                               directions = 8,
                               count_boundary = FALSE,
                               classes_max = NULL,
                               neighbourhood = 4,
                               ordered = TRUE,
                               base = "log2",
                               full_name = FALSE,
                               verbose = TRUE) {

    purrr::map_dfr(landscape,
                   calculate_metrics_internal,
                   what = what,
                   directions = directions,
                   count_boundary = count_boundary,
                   classes_max = classes_max,
                   neighbourhood = neighbourhood,
                   ordered = ordered,
                   base = base,
                   full_name = full_name,
                   verbose = verbose) %>%
        dplyr::mutate(layer = as.integer(layer))
}

calculate_metrics_internal <- function(landscape,
                                   what,
                                   directions,
                                   count_boundary,
                                   classes_max,
                                   neighbourhood,
                                   ordered,
                                   base,
                                   full_name,
                                   verbose) {

    if (any(what %in% c("all", "patch", "class", "landscape"))) {

        if (any(what == "all")) {

            namespace_all <- getNamespaceExports("landscapemetrics")
            namespace_all <- namespace_all[namespace_all %in%
                                                   grep("lsm_", namespace_all,
                                                        value = TRUE)]
            namespace_all <-
                namespace_all[!grepl("\\.|calc", namespace_all)]

            result_all <-
                purrr::map_dfr(namespace_all, function(current_function) {

                    foo <- match.fun(current_function)
                    arguments <- names(formals(foo))
                    do.call(what = foo,
                            args = mget(arguments,
                                        envir = parent.env(environment())))
                })
        }

        if (any(what == "patch")) {

            namespace_patch <- getNamespaceExports("landscapemetrics")
            namespace_patch <- namespace_patch[namespace_patch %in%
                                                   grep("_p_", namespace_patch,
                                                        value = TRUE)]
            namespace_patch <-
                namespace_patch[!grepl("\\.|calc", namespace_patch)]

            result_patch <-
                purrr::map_dfr(namespace_patch, function(current_function) {

                    foo <- match.fun(current_function)
                    arguments <- names(formals(foo))
                    do.call(what = foo,
                            args = mget(arguments,
                                        envir = parent.env(environment())))
                })
        }

        if (any(what == "class")) {
            namespace_class <- getNamespaceExports("landscapemetrics")
            namespace_class <- namespace_class[namespace_class %in%
                                                   grep("_c_", namespace_class,
                                                        value = TRUE)]
            namespace_class <-
                namespace_class[!grepl("\\.|calc", namespace_class)]

            result_class <-
                purrr::map_dfr(namespace_class, function(current_function) {

                    foo <- match.fun(current_function)
                    arguments <- names(formals(foo))
                    do.call(what = foo,
                            args = mget(arguments,
                                        envir = parent.env(environment())))
                })
        }

        if (any(what == "landscape")) {
            namespace_landscape <-
                getNamespaceExports("landscapemetrics")
            namespace_landscape <-
                namespace_landscape[namespace_landscape %in%
                                        grep("_l_", namespace_landscape,
                                             value = TRUE)]
            namespace_landscape <-
                namespace_landscape[!grepl("\\.|calc", namespace_landscape)]

            result_landscape <-
                purrr::map_dfr(namespace_landscape, function(current_function) {

                    foo <- match.fun(current_function)
                    arguments <- names(formals(foo))
                    do.call(what = foo,
                            args = mget(arguments,
                                        envir = parent.env(environment())))
                })
        }

        if(!exists("result_all", inherits = FALSE)){result_all <- tibble::tibble()}
        if(!exists("result_patch", inherits = FALSE)){result_patch <- tibble::tibble()}
        if(!exists("result_class", inherits = FALSE)){result_class <- tibble::tibble()}
        if(!exists("result_landscape", inherits = FALSE)){result_landscape <- tibble::tibble()}

        result_level <- dplyr::bind_rows(result_all,
                                   result_patch,
                                   result_class,
                                   result_landscape)
    }

    if (any(!(what %in% c("all", "patch", "class", "landscape")))) {

        what <- what[!(what %in% c("all", "patch", "class", "landscape"))]

        result_metrics <-
            purrr::map(what, function(current_function) {

                foo <- match.fun(current_function)
                arguments <- names(formals(foo))
                do.call(what = foo,
                        args = mget(arguments,
                                    envir = parent.env(environment())))
            })
    }

    if(!exists("result_level", inherits = FALSE)){result_level <- tibble::tibble()}
    if(!exists("result_metrics", inherits = FALSE)){result_metrics <- tibble::tibble()}

    result <- dplyr::bind_rows(result_level,
                               result_metrics)

    if(full_name == TRUE){
        result <- dplyr::left_join(x = result,
                                   y = lsm_abbreviations_names,
                                   by = "metric")
    }

    result <- dplyr::arrange(result, layer, level, metric, class, id)

    return(result)
}


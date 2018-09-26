#' calculate_lsm
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "all", "patch", "class",
#' "landscape". The default is "all". It is also possible to specify functions
#' as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param count_boundary Include landscape boundary in edge length
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
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
#' @param progress Print progress report
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' calculate_lsm(landscape)
#' calculate_lsm(landscape, what = "patch")
#' calculate_lsm(landscape, what = c("patch", "lsm_c_te", "lsm_l_pr"))
#' }
#'
#' @aliases calculate_lsm
#'
#' @rdname calculate_lsm
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
calculate_lsm <- function(landscape,
                              what,
                              directions,
                              count_boundary,
                              consider_boundary,
                              edge_depth,
                              classes_max,
                              neighbourhood,
                              ordered,
                              base,
                              full_name,
                              verbose,
                              progress) UseMethod("calculate_lsm")

#' @name calculate_lsm
#' @export
calculate_lsm.RasterLayer <- function(landscape,
                                          what = "all",
                                          directions = 8,
                                          count_boundary = FALSE,
                                          consider_boundary = FALSE,
                                          edge_depth = 1,
                                          classes_max = NULL,
                                          neighbourhood = 4,
                                          ordered = TRUE,
                                          base = "log2",
                                          full_name = FALSE,
                                          verbose = TRUE,
                                          progress = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = calculate_lsm_internal,
                     what = what,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress)

    result <- dplyr::bind_rows(result, .id = "layer2")

    dplyr::select(dplyr::mutate(result, layer = as.integer(layer2)),
                  -layer2)
}

#' @name calculate_lsm
#' @export
calculate_lsm.RasterStack <- function(landscape,
                                          what = "all",
                                          directions = 8,
                                          count_boundary = FALSE,
                                          consider_boundary = FALSE,
                                          edge_depth = 1,
                                          classes_max = NULL,
                                          neighbourhood = 4,
                                          ordered = TRUE,
                                          base = "log2",
                                          full_name = FALSE,
                                          verbose = TRUE,
                                          progress = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = calculate_lsm_internal,
                     what = what,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress)

    result <- dplyr::bind_rows(result, .id = "layer2")

    dplyr::select(dplyr::mutate(result, layer = as.integer(layer2)),
                  -layer2)
}

#' @name calculate_lsm
#' @export
calculate_lsm.RasterBrick <- function(landscape,
                                          what = "all",
                                          directions = 8,
                                          count_boundary = FALSE,
                                          consider_boundary = FALSE,
                                          edge_depth = 1,
                                          classes_max = NULL,
                                          neighbourhood = 4,
                                          ordered = TRUE,
                                          base = "log2",
                                          full_name = FALSE,
                                          verbose = TRUE,
                                          progress = FALSE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = calculate_lsm_internal,
                     what = what,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress)

    result <- dplyr::bind_rows(result, .id = "layer2")

    dplyr::select(dplyr::mutate(result, layer = as.integer(layer2)),
                  -layer2)
}

#' @name calculate_lsm
#' @export
calculate_lsm.stars <- function(landscape,
                                    what = "all",
                                    directions = 8,
                                    count_boundary = FALSE,
                                    consider_boundary = FALSE,
                                    edge_depth = 1,
                                    classes_max = NULL,
                                    neighbourhood = 4,
                                    ordered = TRUE,
                                    base = "log2",
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = calculate_lsm_internal,
                     what = what,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress)

    result <- dplyr::bind_rows(result, .id = "layer2")

    dplyr::select(dplyr::mutate(result, layer = as.integer(layer2)),
                  -layer2)
}


#' @name calculate_lsm
#' @export
calculate_lsm.list <- function(landscape,
                                   what = "all",
                                   directions = 8,
                                   count_boundary = FALSE,
                                   consider_boundary = FALSE,
                                   edge_depth = 1,
                                   classes_max = NULL,
                                   neighbourhood = 4,
                                   ordered = TRUE,
                                   base = "log2",
                                   full_name = FALSE,
                                   verbose = TRUE,
                                   progress = FALSE) {

    result <- lapply(X = landscape,
                     FUN = calculate_lsm_internal,
                     what = what,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress)

    result <- dplyr::bind_rows(result, .id = "layer2")

    dplyr::select(dplyr::mutate(result, layer = as.integer(layer2)),
                  -layer2)
}

calculate_lsm_internal <- function(landscape,
                                       what,
                                       directions,
                                       count_boundary,
                                       consider_boundary,
                                       edge_depth,
                                       classes_max,
                                       neighbourhood,
                                       ordered,
                                       base,
                                       full_name,
                                       verbose,
                                       progress) {

    if (any(what %in% c("all", "patch", "class", "landscape"))) {

        if (any(what == "all")) {

            namespace_all <- getNamespaceExports("landscapemetrics")
            namespace_all <- namespace_all[namespace_all %in%
                                               grep("lsm_", namespace_all,
                                                    value = TRUE)]
            namespace_all <- namespace_all[!grepl("\\.|calc", namespace_all)]
            namespace_all <- namespace_all[!grepl("\\.|int", namespace_all)]

            result_all_list <- lapply(seq_along(namespace_all), FUN = function(current_metric) {

                if(isTRUE(progress)){
                    cat("\r> Progress 'all' metrics: ", current_metric, "/",
                        length(namespace_all), "- Current metric: ",
                        namespace_all[[current_metric]], " ")
                }

                foo <- match.fun(namespace_all[[current_metric]])
                arguments <- names(formals(foo))
                do.call(what = foo,
                        args = mget(arguments,
                                    envir = parent.env(environment())))
            })

            result_all <- dplyr::bind_rows(result_all_list)
        }

        if (any(what == "patch")) {

            namespace_patch <- getNamespaceExports("landscapemetrics")
            namespace_patch <- namespace_patch[namespace_patch %in%
                                                   grep("_p_", namespace_patch,
                                                        value = TRUE)]
            namespace_patch <- namespace_patch[!grepl("\\.|calc", namespace_patch)]
            namespace_patch <- namespace_patch[!grepl("\\.|int", namespace_patch)]

            result_patch_list <- lapply(seq_along(namespace_patch), FUN = function(current_metric) {

                if(isTRUE(progress)){
                    cat("\r> Progress 'patch' metrics: ", current_metric, "/",
                        length(namespace_patch), "- Current metric: ",
                        namespace_patch[[current_metric]], " ")
                }

                foo <- match.fun(namespace_patch[[current_metric]])
                arguments <- names(formals(foo))
                do.call(what = foo,
                        args = mget(arguments,
                                    envir = parent.env(environment())))
            })

            result_patch <- dplyr::bind_rows(result_patch_list)
        }

        if (any(what == "class")) {

            namespace_class <- getNamespaceExports("landscapemetrics")
            namespace_class <- namespace_class[namespace_class %in%
                                                   grep("_c_", namespace_class,
                                                        value = TRUE)]
            namespace_class <- namespace_class[!grepl("\\.|calc", namespace_class)]
            namespace_class <- namespace_class[!grepl("\\.|int", namespace_class)]

            result_class_list <- lapply(seq_along(namespace_class), FUN = function(current_metric) {

                if(isTRUE(progress)){
                    cat("\r> Progress 'class' metrics: ", current_metric, "/",
                        length(namespace_class), "- Current metric: ",
                        namespace_class[[current_metric]], " ")
                }

                foo <- match.fun(namespace_class[[current_metric]])
                arguments <- names(formals(foo))
                do.call(what = foo,
                        args = mget(arguments,
                                    envir = parent.env(environment())))
            })

            result_class <- dplyr::bind_rows(result_class_list)
        }

        if (any(what == "landscape")) {

            namespace_landscape <- getNamespaceExports("landscapemetrics")
            namespace_landscape <- namespace_landscape[namespace_landscape %in%
                                                           grep("_l_", namespace_landscape,
                                                                value = TRUE)]
            namespace_landscape <- namespace_landscape[!grepl("\\.|calc", namespace_landscape)]
            namespace_landscape <- namespace_landscape[!grepl("\\.|int", namespace_landscape)]

            result_landscape_list <- lapply(seq_along(namespace_landscape), FUN = function(current_metric) {

                if(isTRUE(progress)){
                    cat("\r> Progress 'landscape' metrics: ", current_metric, "/",
                        length(namespace_landscape), "- Current metric: ",
                        namespace_landscape[[current_metric]], " ")
                }

                foo <- match.fun(namespace_landscape[[current_metric]])
                arguments <- names(formals(foo))
                do.call(what = foo,
                        args = mget(arguments,
                                    envir = parent.env(environment())))
            })

            result_landscape <- dplyr::bind_rows(result_landscape_list)
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

        result_metrics_list <- lapply(seq_along(what), FUN = function(current_metric) {

            if(isTRUE(progress)){
                cat("\r> Progress 'various' metrics: ", current_metric, "/",
                    length(what), "- Current metric: ",
                    what[[current_metric]], " ")
            }

            foo <- match.fun(what[[current_metric]])
            arguments <- names(formals(foo))
            do.call(what = foo,
                    args = mget(arguments,
                                envir = parent.env(environment())))
        })

        result_metrics <- dplyr::bind_rows(result_metrics_list)
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

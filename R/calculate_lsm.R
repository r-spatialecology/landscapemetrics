#' calculate_lsm
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param level Level of metrics to calculate (e.g. 'landscape').
#' @param metric Abbreviation of metrics to calculate (e.g. 'area').
#' @param name Full name of metrics to calculate (e.g. 'core area').
#' @param type Metric types to calculate according to FRAGSTATS grouping (e.g. 'aggregation metric').
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
#' @param verbose Print warning messages
#' @param progress Print progress report
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' calculate_lsm(landscape)
#' calculate_lsm(landscape, what = c("patch", "lsm_c_te", "lsm_l_pr"))
#' calculate_lsm(landscape, level = c("class", "landscape"), type = "aggregation metric")
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
                          level,
                          metric,
                          name,
                          type,
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
                                      what = NULL,
                                      level = NULL,
                                      metric = NULL,
                                      name = NULL,
                                      type = NULL,
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
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
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

    result <- dplyr::bind_rows(result, .id = "layer")

    result <- dplyr::arrange(result,
                             layer, level, metric, class, id)

    return(result)
}

#' @name calculate_lsm
#' @export
calculate_lsm.RasterStack <- function(landscape,
                                      what = NULL,
                                      level = NULL,
                                      metric = NULL,
                                      name = NULL,
                                      type = NULL,
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
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
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

    result <- dplyr::bind_rows(result, .id = "layer")

    result <- dplyr::arrange(result,
                             layer, level, metric, class, id)

    return(result)
}

#' @name calculate_lsm
#' @export
calculate_lsm.RasterBrick <- function(landscape,
                                      what = NULL,
                                      level = NULL,
                                      metric = NULL,
                                      name = NULL,
                                      type = NULL,
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
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
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

    result <- dplyr::bind_rows(result, .id = "layer")

    result <- dplyr::arrange(result,
                             layer, level, metric, class, id)

    return(result)
}

#' @name calculate_lsm
#' @export
calculate_lsm.stars <- function(landscape,
                                what = NULL,
                                level = NULL,
                                metric = NULL,
                                name = NULL,
                                type = NULL,
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
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
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

    result <- dplyr::bind_rows(result, .id = "layer")

    result <- dplyr::arrange(result,
                             layer, level, metric, class, id)

    return(result)
}


#' @name calculate_lsm
#' @export
calculate_lsm.list <- function(landscape,
                               what = NULL,
                               level = NULL,
                               metric = NULL,
                               name = NULL,
                               type = NULL,
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
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
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

    result <- dplyr::bind_rows(result, .id = "layer")

    result <- dplyr::arrange(result,
                             layer, level, metric, class, id)

    return(result)
}

calculate_lsm_internal <- function(landscape,
                                   what,
                                   level,
                                   metric,
                                   name,
                                   type,
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

    # get name of metrics
    metrics <- landscapemetrics::list_lsm(level = level,
                                          metric = metric,
                                          name = name,
                                          type = type,
                                          what = what,
                                          simplify = TRUE,
                                          verbose = verbose)

    # use internal functions for calculation
    metrics_calc <- paste0(metrics, "_calc")

    # how many metrics need to be calculated?
    number_metrics <- length(metrics_calc)

    # properties of original raster
    extent <- raster::extent(landscape)
    resolution <- raster::res(landscape)
    crs <- raster::crs(landscape)

    # convert to matrix
    landscape <- raster::as.matrix(landscape)

    result_all_list <- lapply(seq_along(metrics_calc), FUN = function(current_metric) {

        # print progess using the non-internal name
        if(isTRUE(progress)){
            cat("\r> Progress: ", current_metric, "/",
                number_metrics, "- Current metric: ",
                metrics[[current_metric]], " ")
        }

        # match function name
        foo <- get(metrics_calc[[current_metric]], mode = "function")

        # get argument
        arguments <- names(formals(foo))

        # run function
        do.call(what = foo,
                args = mget(arguments, envir = parent.env(environment())))
    })

    result <- dplyr::bind_rows(result_all_list)

    if(full_name == TRUE){
        result <- dplyr::left_join(x = result,
                                   y = landscapemetrics::lsm_abbreviations_names,
                                   by = c("metric", "level"))
    }

    return(result)
}

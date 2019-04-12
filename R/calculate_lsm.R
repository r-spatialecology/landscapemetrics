#' calculate_lsm
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param level Level of metrics. Either 'patch', 'class' or 'landscape' (or vector with combination).
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
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
#' @details
#' Wrapper to calculate several landscape metrics. The metrics can be specified
#' by the arguments `what`, `level`, `metric`, `name` and/or `type` (combinations
#' of different arguments are possible (e.g. `level = "class", type = "aggregation metric"`).
#' If an argument is not provided, automatically all possibilities are
#' selected. Therefore, to get **all** available metrics, don't specify any of the
#' above arguments.
#'
#' @seealso
#' \code{\link{list_lsm}}
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' calculate_lsm(landscape, progress = TRUE)
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

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- result[with(result, order(layer, level, metric, class, id)), ]

    tibble::add_column(result, layer, .before = TRUE)
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

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)

            progress <- FALSE
        }

        calculate_lsm_internal(landscape = landscape[[x]],
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
                               progress = progress)
        })

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- result[with(result, order(layer, level, metric, class, id)), ]

    if (progress) {message("")}

    tibble::add_column(result, layer, .before = TRUE)
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

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)

            progress <- FALSE
        }

        calculate_lsm_internal(landscape = landscape[[x]],
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
                               progress = progress)
    })

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- result[with(result, order(layer, level, metric, class, id)), ]

    if (progress) {message("")}

    tibble::add_column(result, layer, .before = TRUE)
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

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- result[with(result, order(layer, level, metric, class, id)), ]

    tibble::add_column(result, layer, .before = TRUE)
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

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)

            progress <- FALSE
        }

        calculate_lsm_internal(landscape = landscape[[x]],
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
                               progress = progress)
    })

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result <- result[with(result, order(layer, level, metric, class, id)), ]

    if (progress) {message("")}

    tibble::add_column(result, layer, .before = TRUE)
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
    metrics <- list_lsm(level = level,
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

    # get coordinates of cells
    points <- raster_to_points(landscape)[, 2:4]

    # resolution of original raster
    resolution <- raster::res(landscape)

    # convert to matrix
    landscape <- raster::as.matrix(landscape)

    result <- do.call(rbind, lapply(seq_along(metrics_calc), FUN = function(current_metric) {

        # print progess using the non-internal name
        if (isTRUE(progress)) {

            message("\r> Progress metrics: ", current_metric, "/",
                    number_metrics, appendLF = FALSE)
        }

        # match function name
        foo <- get(metrics_calc[[current_metric]], mode = "function")

        # get argument
        arguments <- names(formals(foo))

        # run function
        do.call(what = foo,
                args = mget(arguments, envir = parent.env(environment())))
        })
    )

    if (full_name == TRUE) {

        col_ordering <- c("level", "class", "id", "metric", "value",
                          "name", "type", "function_name")

        result <- merge(x = result,
                        y = lsm_abbreviations_names,
                        by = c("level", "metric"),
                        all.x = TRUE, sort = FALSE, suffixes = c("", ""))

        result <- tibble::as_tibble(result[ ,col_ordering])
    }

    if (progress) {
        message("")
    }

    return(result)
}

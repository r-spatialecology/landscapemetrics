#' spatialize_lsm
#'
#' @description Spatialize landscape metric values
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
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.

#'
#' @details
#' The functions returns a nested list with \code{RasterLayer}s. The first level
#' contains each input layer (only one element if \code{RasterLayer} was provided).
#' The second level contains a \code{RasterLayer} for each selected metric
#' (see \code{list_lsm} for details) where each cell has the landscape metric
#' value of the patch it belongs to. Only patch level metrics are allowed.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{show_lsm}}
#'
#' @return list
#'
#' @examples
#' spatialize_lsm(landscape, what = "lsm_p_area")
#'
#' @aliases spatialize_lsm
#'
#' @rdname spatialize_lsm
#'
#' @export
spatialize_lsm <- function(landscape,
                           level, metric, name, type, what,
                           directions,
                           progress,
                           ...) UseMethod("spatialize_lsm")

#' @name spatialize_lsm
#' @export
spatialize_lsm.RasterLayer <- function(landscape,
                                       level = "patch",
                                       metric = NULL,
                                       name = NULL,
                                       type = NULL,
                                       what = NULL,
                                       directions = 8,
                                       progress = FALSE,
                                       ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = spatialize_lsm_internal,
                     level = level,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     directions = directions,
                     progress = progress,
                     ...)

    return(result)
}

#' @name spatialize_lsm
#' @export
spatialize_lsm.RasterStack <- function(landscape,
                                       level = "patch",
                                       metric = NULL,
                                       name = NULL,
                                       type = NULL,
                                       what = NULL,
                                       directions = 8,
                                       progress = FALSE,
                                       ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        spatialize_lsm_internal(landscape = landscape[[x]],
                                level = level,
                                metric = metric,
                                name = name,
                                type = type,
                                what = what,
                                directions = directions,
                                progress = FALSE,
                                ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name spatialize_lsm
#' @export
spatialize_lsm.RasterBrick <- function(landscape,
                                       level = "patch",
                                       metric = NULL,
                                       name = NULL,
                                       type = NULL,
                                       what = NULL,
                                       directions = 8,
                                       progress = FALSE,
                                       ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        spatialize_lsm_internal(landscape = landscape[[x]],
                                level = level,
                                metric = metric,
                                name = name,
                                type = type,
                                what = what,
                                directions = directions,
                                progress = FALSE,
                                ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name spatialize_lsm
#' @export
spatialize_lsm.stars <- function(landscape,
                                 level = "patch",
                                 metric = NULL,
                                 name = NULL,
                                 type = NULL,
                                 what = NULL,
                                 directions = 8,
                                 progress = FALSE,
                                 ...) {

    landscape <- raster::as.list(methods::as(landscape, "Raster"))

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        spatialize_lsm_internal(landscape = landscape[[x]],
                                level = level,
                                metric = metric,
                                name = name,
                                type = type,
                                what = what,
                                directions = directions,
                                progress = FALSE,
                                ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

#' @name spatialize_lsm
#' @export
spatialize_lsm.list <- function(landscape,
                                level = "patch",
                                metric = NULL,
                                name = NULL,
                                type = NULL,
                                what = NULL,
                                directions = 8,
                                progress = FALSE,
                                ...) {

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            cat("\r> Progress nlayers: ", x , "/", length(landscape))
        }

        spatialize_lsm_internal(landscape = landscape[[x]],
                                level = level,
                                metric = metric,
                                name = name,
                                type = type,
                                what = what,
                                directions = directions,
                                progress = FALSE,
                                ...)
    })

    if (progress) {cat("\n")}

    return(result)
}

spatialize_lsm_internal <- function(landscape,
                                    level, metric, name, type, what,
                                    directions,
                                    progress,
                                    ...) {

    # get name of metrics
    metrics <- list_lsm(level = level,
                        metric = metric,
                        name = name,
                        type = type,
                        what = what,
                        simplify = TRUE,
                        verbose = FALSE)

    # how many metrics need to be calculated?
    number_metrics <- length(metrics)

    # error if no patch level metrics are provided
    if (!all(metrics %in% list_lsm(level = "patch", simplify = TRUE))) {
        stop("'spatialize_lsm()' only takes patch level metrics.",
             call. = FALSE)
    }

    # get CRS of input
    crs_input <- raster::crs(landscape)

    # get patches
    landscape_labeled <- get_patches(landscape, directions = directions)

    # continious, unique patch id
    for (i in seq_len(length(landscape_labeled) - 1)) {

        max_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_id
    }

    # get dataframe with patch ID and coordinates to merge with result of metric
    patches_tibble <- raster::as.data.frame(sum(raster::stack(landscape_labeled),
                                                na.rm = TRUE),
                                            xy = TRUE)

    # modify names
    names(patches_tibble) <- c("x", "y", "id")

    # replace all 0 values for NA
    patches_tibble$id <- replace(patches_tibble$id,
                                 patches_tibble$id == 0,
                                 NA)

    # create object for warning messages
    warning_messages <- character(0)

    # loop through metrics and return raster with value for each patch
    result <- withCallingHandlers(expr = {lapply(seq_along(metrics), function(x) {

        # print progess using the non-internal name
        if (progress) {

            cat("\r> Progress metrics: ", x, "/", number_metrics)
        }

        # get metric value
        fill_value <- calculate_lsm(landscape,
                                    what = metrics[[x]],
                                    progress = FALSE,
                                    ...)

        # merge with coords data frame
        fill_value <- merge(x = patches_tibble,
                            y = fill_value,
                            by = "id",
                            all.x = TRUE)

        # convert to raster (wrap )
        raster::rasterFromXYZ(fill_value[, c(2, 3, 8)], crs = crs_input)
    })},
    warning = function(cond) {

        warning_messages <<- c(warning_messages, conditionMessage(cond))

        invokeRestart("muffleWarning")})

    # using metrics to name list
    names(result) <- metrics

    if (progress) {cat("\n")}

    # warnings present
    if (length(warning_messages) > 0) {

        # only unique warnings
        warning_messages <- unique(warning_messages)

        # print warnings
        lapply(warning_messages, function(x){ warning(x, call. = FALSE)})
    }

    return(result)
}

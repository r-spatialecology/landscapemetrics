#' get_lsm
#'
#' @description Get landscape metric values
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
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
#' get_lsm(landscape, what = "lsm_p_area")
#'
#' @aliases get_lsm
#'
#' @rdname get_lsm
#'
#' @export
get_lsm <- function(landscape,
                    metric, name, type, what,
                    directions,
                    progress,
                    ...) UseMethod("get_lsm")

#' @name get_lsm
#' @export
get_lsm.RasterLayer <- function(landscape,
                                metric = NULL,
                                name = NULL,
                                type = NULL,
                                what = NULL,
                                directions = 8,
                                progress = FALSE,
                                ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = get_lsm_internal,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     directions = directions,
                     progress = progress,
                     ...)

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.RasterStack <- function(landscape,
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

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        get_lsm_internal(landscape = landscape[[x]],
                         metric = metric,
                         name = name,
                         type = type,
                         what = what,
                         directions = directions,
                         progress = FALSE,
                         ...)
    })

    if (progress) {message("")}

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.RasterBrick <- function(landscape,
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

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        get_lsm_internal(landscape = landscape[[x]],
                         metric = metric,
                         name = name,
                         type = type,
                         what = what,
                         directions = directions,
                         progress = FALSE,
                         ...)
    })

    if (progress) {message("")}

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.stars <- function(landscape,
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

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        get_lsm_internal(landscape = landscape[[x]],
                         metric = metric,
                         name = name,
                         type = type,
                         what = what,
                         directions = directions,
                         progress = FALSE,
                         ...)
    })

    if (progress) {message("")}

    return(result)
}

#' @name get_lsm
#' @export
get_lsm.list <- function(landscape,
                         metric = NULL,
                         name = NULL,
                         type = NULL,
                         what = NULL,
                         directions = 8,
                         progress = FALSE,
                         ...) {

    result <- lapply(X = seq_along(landscape), FUN = function(x) {

        if (progress) {

            message("\r> Progress nlayers: ", x , "/", length(landscape),
                    appendLF = FALSE)
        }

        get_lsm_internal(landscape = landscape[[x]],
                         metric = metric,
                         name = name,
                         type = type,
                         what = what,
                         directions = directions,
                         progress = FALSE,
                         ...)
    })

    if (progress) {message("")}

    return(result)
}

get_lsm_internal <- function(landscape,
                             metric, name, type, what,
                             directions,
                             progress,
                             ...) {

    # get name of metrics
    metrics <- list_lsm(level = "patch",
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
        stop("'get_lsm()' only takes patch level metrics.",
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

    # loop through metrics and return raster with value for each patch
    result <- lapply(seq_along(metrics), function(x) {

        # print progess using the non-internal name
        if (progress) {

            message("\r> Progress metrics: ", x, "/",
                    number_metrics, appendLF = FALSE)
        }

        # get metric value
        fill_value <- calculate_lsm(landscape,
                                    what = metrics[[x]],
                                    progress = FALSE,
                                    ...)

        # merge with coords data frame
        fill_value <-  merge(x = patches_tibble,
                             y = fill_value,
                             by = "id",
                             all.x = TRUE)

        # convert to raster
        raster::rasterFromXYZ(fill_value[, c(2,3, 8)], crs = crs_input)
    })

    # using metrics to name list
    names(result) <- metrics

    if (progress) {

        message("")
    }

    return(result)
}

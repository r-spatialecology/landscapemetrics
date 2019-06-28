#' scale_window
#'
#' @description Metrics on changing sample scale
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param percentages_col 2-column matrix with coordinates or SpatialPoints.
#' @param percentages_row String specifying plot shape. Either "circle" or "square"
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_l_mutinf", "lsm_l_ta")`.
#' @param stat The function to be applied. See Details
#' @param verbose If TRUE, warnings are printed.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.
#'
#' @details
#' This function calculates the selected metrics in moving windows over the provided
#' landscape.
#'
#' Please be aware that the output is sligthly different to all other `lsm`-function
#' of `landscapemetrics`.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Only metrics on landscape level
#' are supported for this function.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{window_lsm}} \cr
#' \code{\link{scale_sample}}
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' percentages_col <- c(2, 4, 8, 16, 32, 64, 100)
#' percentages_row <- c(2, 4, 8, 16, 32, 64, 100)
#'
#' what =  c("lsm_l_pr", "lsm_l_joinent")
#'
#' stat <- "mean"
#'
#' scale_window(landscape, percentages_col, percentages_row, what, stat)
#' }
#'
#' @aliases scale_window
#' @rdname scale_window
#'
#' @export
scale_window <- function(landscape,
                         percentages_col,
                         percentages_row,
                         what,
                         stat,
                         verbose,
                         progress,
                         ...) UseMethod("scale_window")

#' @name scale_window
#' @export
scale_window.RasterLayer <- function(landscape,
                                     percentages_col = c(2, 4, 8, 16, 32, 64, 100),
                                     percentages_row = NULL,
                                     what,
                                     stat,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = scale_window_int,
                     percentages_col = percentages_col,
                     percentages_row = percentages_row,
                     what = what,
                     stat = stat,
                     verbose = verbose,
                     progress = progress,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    tibble::as_tibble(result[with(result, order(layer, level, metric, class, id, percentages_col, percentages_row)),
                             c(8,5,6,7,2,1,3,4)])

}

#' @name scale_window
#' @export
scale_window.RasterStack <- function(landscape,
                                     percentages_col = c(2, 4, 8, 16, 32, 64, 100),
                                     percentages_row = NULL,
                                     what,
                                     stat,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(
        X = seq_along(landscape),
        FUN = function(x) {
            if (progress) {
                message("\r> Progress nlayers: ",
                        x ,
                        "/",
                        length(landscape),
                        appendLF = FALSE)
            }

            scale_window_int(
                landscape = landscape[[x]],
                percentages_col = percentages_col,
                percentages_row = percentages_row,
                what = what,
                stat = stat,
                verbose = verbose,
                progress = FALSE
            )
        }
    )

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    tibble::as_tibble(result[with(result, order(layer, level, metric, class, id, percentages_col, percentages_row)),
                             c(8,5,6,7,2,1,3,4)])
}

#' @name scale_window
#' @export
scale_window.RasterBrick <- function(landscape,
                                     percentages_col = NULL,
                                     percentages_row = NULL,
                                     what,
                                     stat,
                                     verbose = TRUE,
                                     progress = FALSE,
                                     ...) {

    landscape <- raster::as.list(landscape)

    result <- lapply(
        X = seq_along(landscape),
        FUN = function(x) {
            if (progress) {
                message("\r> Progress nlayers: ",
                        x ,
                        "/",
                        length(landscape),
                        appendLF = FALSE)
            }

            scale_window_int(
                landscape = landscape[[x]],
                percentages_col = percentages_col,
                percentages_row = percentages_row,
                what = what,
                stat = stat,
                verbose = verbose,
                progress = FALSE
            )
        }
    )

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    tibble::as_tibble(result[with(result, order(layer, level, metric, class, id, percentages_col, percentages_row)),
                             c(8,5,6,7,2,1,3,4)])
}

#' @name scale_window
#' @export
scale_window.stars <- function(landscape,
                               percentages_col = NULL,
                               percentages_row = NULL,
                               what,
                               stat,
                               verbose = TRUE,
                               progress = FALSE,
                               ...) {

    landscape <-  raster::as.list(methods::as(landscape, "Raster"))

    result <- lapply(
        X = seq_along(landscape),
        FUN = function(x) {
            if (progress) {
                message("\r> Progress nlayers: ",
                        x ,
                        "/",
                        length(landscape),
                        appendLF = FALSE)
            }

            scale_window_int(
                landscape = landscape[[x]],
                percentages_col = percentages_col,
                percentages_row = percentages_row,
                what = what,
                stat = stat,
                verbose = verbose,
                progress = FALSE
            )
        }
    )

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    tibble::as_tibble(result[with(result, order(layer, level, metric, class, id, percentages_col, percentages_row)),
                             c(8,5,6,7,2,1,3,4)])
}

#' @name scale_window
#' @export
scale_window.list <- function(landscape,
                              percentages_col = NULL,
                              percentages_row = NULL,
                              what,
                              stat,
                              verbose = TRUE,
                              progress = FALSE,
                              ...) {

    result <- lapply(
        X = seq_along(landscape),
        FUN = function(x) {
            if (progress) {
                message("\r> Progress nlayers: ",
                        x ,
                        "/",
                        length(landscape),
                        appendLF = FALSE)
            }

            scale_window_int(
                landscape = landscape[[x]],
                percentages_col = percentages_col,
                percentages_row = percentages_row,
                what = what,
                stat = stat,
                verbose = verbose,
                progress = FALSE
            )
        }
    )

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {message("")}

    tibble::as_tibble(result[with(result, order(layer, level, metric, class, id, percentages_col, percentages_row)),
                             c(8,5,6,7,2,1,3,4)])
}

scale_window_int <- function(landscape,
                             percentages_col,
                             percentages_row,
                             what,
                             stat,
                             verbose,
                             progress,
                             ...) {

    # only percentages_col provided
    if (is.null(percentages_row)) {

        percentages_row <- percentages_col
    }

    # get dimensions of raster
    ncols <- raster::ncol(landscape)
    nrows <- raster::nrow(landscape)

    # calculate percentage of cols
    ncols_perc  <- round((percentages_col / 100) * ncols)

    # calculate percentage of rows
    nrows_perc  <- round((percentages_row / 100) * nrows)

    # warning if window would be smaller than 3 x 3
    if (any(ncols_perc < 3) | any(nrows_perc < 3)) {

        ncols_perc[which(ncols_perc < 3)] <- 3
        nrows_perc[which(nrows_perc < 3)] <- 3

        if (verbose) {
            warning("The percentages produced a moving window with a side < 3 cells. scale_window set this side to 3 for this scale.",
                    call. = FALSE)
        }
    }

    # add one col to even ncols_perc/nrows_perc
    ncols_perc[ncols_perc %% 2 == 0] <- ncols_perc[ncols_perc %% 2 == 0] + 1
    nrows_perc[nrows_perc %% 2 == 0] <- nrows_perc[nrows_perc %% 2 == 0] + 1

    # make sure ncols_perc/nrows_perc can't be bigger than landscape
    if (any(ncols_perc > ncols) | any(nrows_perc > nrows)) {

        ncols_perc[ncols_perc > ncols] <- ncols - 1
        nrows_perc[nrows_perc > nrows] <- nrows - 1

        if (verbose) {
            warning("The percentages produced a moving window larger than the landscape. scale_window set this to the next smaller uneven number.",
                    call. = FALSE)
        }

    }

    result <- do.call("rbind", lapply(seq_along(ncols_perc), FUN = function(i) {

        window <- matrix(data = 1,
                         nrow = nrows_perc[i],
                         ncol = ncols_perc[i])

        win_raster <- window_lsm(landscape,
                                 window = window,
                                 what = what,
                                 ...)

        win_raster <- unlist(win_raster)

        value <- sapply(seq_along(win_raster), function(i) {

            raster::cellStats(win_raster[[i]],
                              stat = stat,
                              na.rm = TRUE)
        })

        if (progress) {

            message("\r> Progress scales: ", i, "/", length(ncols_perc),
                    appendLF = FALSE)
        }

        value <- as.data.frame(value)

        value$metric <- names(win_raster)

        value$percentages_col <- percentages_col[i]
        value$percentages_row <- percentages_row[i]

        return(value)
    }))

    result$metric <- vapply(strsplit(result$metric, split = "_"), function(x) x[3],
                            FUN.VALUE = character(1))

    result$level <- "landscape"
    result$class <- NA
    result$id <- NA

    if (progress) {

        message("")
    }

    return(result)
}

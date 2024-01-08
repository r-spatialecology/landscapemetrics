#' spatialize_lsm
#'
#' @description Spatialize landscape metric values
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param level Level of metrics. Either 'patch', 'class' or 'landscape' (or vector with combination).
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param progress Print progress report.
#' @param to_disk If TRUE raster will be saved to disk.
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
#' landscape <- terra::rast(landscapemetrics::landscape)
#' p_area_raster <- spatialize_lsm(landscape, what = "lsm_p_area")
#' terra::plot(p_area_raster[[1]][[1]])
#'
#' @export
spatialize_lsm <- function(landscape, level = "patch", metric = NULL, name = NULL,
                           type = NULL, what = NULL, directions = 8, progress = FALSE,
                           to_disk = getOption("to_disk", default = FALSE),
                           ...) {

    landscape <- landscape_as_list(landscape)

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
                                to_disk = to_disk,
                                ...)
    })

    if (progress) {cat("\n")}

    names(result) <- paste0("layer_", 1:length(result))

    return(result)
}

spatialize_lsm_internal <- function(landscape, level, metric, name, type, what,
                                    directions, progress, to_disk, ...) {

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
    crs_input <- terra::crs(landscape)

    # get patches
    landscape_labeled <- get_patches(landscape,
                                     class = "all",
                                     directions = directions,
                                     to_disk = to_disk,
                                     return_raster = TRUE)[[1]]

    # get dataframe with patch ID and coordinates to merge with result of metric
    # MH: Do we really want to remove NA?
    patches_tibble <- terra::as.data.frame(sum(terra::rast(landscape_labeled),
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

        # print progress using the non-internal name
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

        if (to_disk) {

            # order fill_value by x
            index <- order(fill_value$x)
            fill_value <- fill_value[index, ]

            # split by y
            fill_value <- rev(split(x = fill_value, f = fill_value$y))

            # create empty raster
            out <- terra::rast(landscape)

            # starting to write values in raster
            blks <- terra::writeStart(x = out, filename = paste0(tempfile(), ".tif"),
                                      overwrite = TRUE)

            # loop through all block sizes
            for (i in 1:blks$n) {

                # start and end row of current block
                start_row <- blks$row[i]
                end_row <- blks$row[i] + (blks$nrows[i] - 1)

                # get values of current rows and combine to df
                values_temp <- do.call("rbind", fill_value[start_row:end_row])

                terra::writeValues(out, values_temp$value, blks$row[i], blks$nrows[i])
            }

            terra::writeStop(out)

            return(out)
        } else {

            # convert to raster (wrap)
            out <- terra::rast(fill_value[, c(2, 3, 8)], crs = crs_input)

            return(out)
        }})}, warning = function(cond) {

            warning_messages <<- c(warning_messages, conditionMessage(cond))

            invokeRestart("muffleWarning")}
    )

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

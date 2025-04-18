#' scale_sample
#'
#' @description Metrics on changing sample scale
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param y Point geometry as SpatVector or sf object or 2-column matrix with coordinates.
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Approximated size of sample plot. Equals the radius for circles or half of
#' the side-length for squares in mapunits. For lines size equals the width of the buffer.
#' @param transform Logical if planar CRS are transformed to lon/lat for accuracy during area
#' calculations of buffer areas.
#' @param verbose Print warning messages.
#' @param progress Print progress report.
#' @param ... Arguments passed on to \code{calculate_lsm()}.

#' @details
#' This function calculates the selected metrics in sub-sequential buffers around
#' point(s) of interest. To see more details about arguments passed on to the metrics,
#' please see `calculate_lsm()`.
#'
#' The metrics can be specified by the arguments `what`, `level`, `metric`, `name`
#' and/or `type` (combinations of different arguments are possible (e.g.
#' `level = "class", type = "aggregation metric"`). If an argument is not provided,
#' automatically all possibilities are selected. Therefore, to get **all**
#' available metrics, don't specify any of the above arguments.
#'
#' For all metrics based on distances or areas please make sure your data is valid
#' using \code{\link{check_landscape}}.
#'
#' Please be aware that the output is slightly different to all other `lsm`-function
#' of `landscapemetrics`.
#'
#' The size of the actual sampled landscape can be different to the provided size
#' due to two reasons. Firstly, because clipping raster cells using a circle or a
#' sample plot not directly at a cell center lead to inaccuracies. Secondly, sample
#' plots can exceed the landscape boundary. Therefore, we report the actual clipped
#' sample plot area relative in relation to the theoretical, maximum sample plot
#' area e.g. a sample plot only half within the landscape will have a `percentage_inside = 50`.
#' Additionally, if the polygon representing the sample plot is smaller than the cell
#' size of the raster, the `percentage_inside` may exceed 100%. To calculate the area of
#' the buffer zones, the function `terra::expanse()` is used. The area results may
#' be influenced by the CRS and the `transform` argument.
#'
#' @seealso
#' \code{\link{list_lsm}} \cr
#' \code{\link{calculate_lsm}} \cr
#' \code{\link{sample_lsm}} \cr
#' \code{\link{construct_buffer}}
#'
#' @return tibble
#'
#' @examples
#' my_points <- matrix(c(1265000, 1250000, 1255000, 1257000), ncol = 2, byrow = TRUE)
#' my_points <- terra::vect(my_points, crs = terra::rast(landscapemetrics::augusta_nlcd))
#'
#' scale_sample(landscape = terra::rast(landscapemetrics::augusta_nlcd), y = my_points,
#' size = c(500, 750, 1000), what = c("lsm_l_ent", "lsm_l_mutinf"))
#'
#' @aliases scale_sample
#' @rdname scale_sample
#'
#' @export
scale_sample <- function(landscape, y, shape = "square", size, transform = TRUE,
                         verbose = TRUE, progress = FALSE, ...) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = seq_along(landscape), FUN = function(x) {
        if (progress) {cat("\r> Progress nlayers: ", x , "/", length(landscape))}
        scale_sample_int_multibuffer(landscape = landscape[[x]], y = y,
                                     shape = shape, size = size, transform = transform,
                                     verbose = verbose, progress = FALSE,
                                     ...)
    })

    layer <- rep(seq_along(result), vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    if (progress) {cat("\n")}

    # return warning of only 3/4 of sample plot are in landscape
    if (verbose) {
        if (any(result$percentage_inside < 90)) {
            warning("The 'perecentage_inside' is below 90% for at least one buffer.",
                    call. = FALSE)
        }
    }

    result[with(result, order(layer, plot_id, level, metric, class, id, size)), ]
}

scale_sample_int_multibuffer <- function(landscape, y, shape, size, transform, verbose, progress, ...) {

    # loop through buffers
    result <- do.call(rbind, lapply(X = seq_along(size), FUN = function(x) {

        # print progess using the non-internal name
        if (progress) {cat("\r> Progress scales: ", x, "/", length(size))}

        scale_sample_int(landscape = landscape, y = y, shape = shape, size = size[[x]],
                         transform = transform, verbose = verbose, ...)
    }))

    if (progress) {cat("\n")}

    return(result)
}

scale_sample_int <- function(landscape, y, shape, size, transform, verbose, progress, ...) {

    # check if y is spatial object
    if (inherits(x = y, what = c("sf", "sfc", "sfg", "SpatialPoints", "SpatialPolygons", "SpatVector"))) {

        # convert to terra
        y <- methods::as(y, "SpatVector")

        # check of points
        if (terra::geomtype(y) != "points") stop("landscapemetrics currently only supports point features.", call. = FALSE)

        # get crs
        crs <- terra::crs(y)

    # y should be matrix
    } else {

        crs <- ""

    }

    # construct plot area around sample sample_points
    y <- construct_buffer(coords = y, shape = shape, size = size, crs = crs, verbose = verbose)

    # get area of all polygons
    maximum_area <- suppressWarnings(terra::expanse(y, transform = transform)) / 10000

    # loop through each sample point and calculate metrics
    result <- do.call(rbind, lapply(X = seq_along(y), FUN = function(current_plot) {

        # crop sample plot
        landscape_crop <- raster::crop(x = landscape, y = y[current_plot])

        # mask sample plot
        landscape_mask <- raster::mask(x = landscape_crop, mask = y[current_plot])

        # calculate actual area of sample plot
        area <- lsm_l_ta_calc(landscape_mask, directions = 8)

        # calculate lsm
        result_current_plot <- calculate_lsm(landscape = landscape_mask,
                                             verbose = verbose, progress = FALSE,
                                             ...)
        # add buffer size
        result_current_plot$size <- size

        # add plot id
        result_current_plot$plot_id <- current_plot

        # calculate ratio between actual area and theoretical area
        result_current_plot$percentage_inside <- area$value / maximum_area[[current_plot]] * 100

        return(result_current_plot)

    }))

    return(result)

}

#' points_as_mat
#'
#' @description Points as matrix
#'
#' @param pts SpatVector points or sf object
#'
#' @details
#' Converts sf points to coordinates matrix
#'
#' @return matrix
#'
#' @aliases points_as_mat
#' @rdname points_as_mat
#'
#' @keywords internal
#'
#' @export
points_as_mat = function(pts) {

    # convert to coords if sf object is provided
    if (inherits(x = pts, what = "sf") | inherits(x = pts, what = "sfc") | inherits(x = pts, what = "sfg") |
        inherits(x = pts, what = "SpatVector")) {

        # convert to terra
        pts <- as(pts, "SpatVector")

        # check of points
        if (terra::geomtype(pts) != "points") stop("landscapemetrics currently only supports point or polygon features.",
                                                   call. = FALSE)

        # get coords
        mat <- matrix(terra::crds(pts), ncol = 2)

        return(mat)

    # already matrix provided
    } else if (inherits(x = pts, what = "matrix")) {

        # return error if not just two cols
        if (ncol(pts) != 2) stop("Please provide a matrix with coords, point or polygon  object.", call. = FALSE)

        return(pts)

    # not supported class at all
    } else {

        stop("'y' must be a matrix, SpatVecotr, or sf object.", call. = FALSE)

    }
}

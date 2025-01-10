#' points_as_mat
#'
#' @description Points as matrix
#'
#' @param pts Point geometry as SpatVector or sf object.
#'
#' @details
#' Converts sf points to coordinates matrix
#'
#' @return matrix
#'
#' @keywords internal
#'
#' @export
points_as_mat = function(pts) {

    # convert to coords if sf object is provided
    if (inherits(x = pts, what = c("sf", "sfc", "sfg", "SpatialPoints", "SpatVector"))) {

        # convert to terra
        pts <- methods::as(pts, "SpatVector")

        # check of points
        if (terra::geomtype(pts) != "points") stop("landscapemetrics currently only supports point features.",
                                                   call. = FALSE)

        # get coords
        mat <- matrix(terra::crds(pts), ncol = 2)

        return(mat)

    # already matrix provided
    } else if (inherits(x = pts, what = "matrix")) {

        # return error if not just two cols
        if (ncol(pts) != 2) stop("Please provide a matrix with coords or point object.", call. = FALSE)

        return(pts)

    # not supported class at all
    } else {

        stop("'y' must be a matrix, SpatVector, or sf object.", call. = FALSE)

    }
}

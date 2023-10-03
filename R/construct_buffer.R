#' construct_buffer
#'
#' @description Internal function to construct plot area around coordinates
#'
#' @param coords SpatVector, sf object or 2-column matrix with coordinates of sample points
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Size of sample plot. Equals the radius for circles or the
#' side-length for squares in map units
#' @param return_vec If TRUE, vector objects are returned.
#' @param verbose Print warning messages.
#'
#' @return
#' matrix or sf objecct
#'
#' @examples
#' coords <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' construct_buffer(coords = coords, shape = "square", size = 5)
#'
#' @aliases construct_buffer
#' @rdname construct_buffer
#'
#' @keywords internal
#'
#' @export
construct_buffer <- function(coords, shape , size, return_vec = TRUE, verbose = TRUE) {

    # make sure coords are matrix
    coords <- points_as_mat(pts = coords)

    if (verbose) {

        if (ncol(coords) != 2) {

            warning("'coords' should be a two column matrix including x- and y-coordinates.",
                    call. = FALSE)
        }
    }

    if (shape == "circle") {

        circle_points_x <- sin(seq(0, 2 * pi, length.out = 100)) * size
        circle_points_y <- cos(seq(0, 2 * pi, length.out = 100)) * size

        x_circle <- outer(circle_points_x,  coords[, 1], `+`)
        y_circle <- outer(circle_points_y,  coords[, 2], `+`)

        sample_plots <- cbind(matrix(x_circle, ncol = 1),
                              matrix(y_circle, ncol = 1),
                              rep(seq_len(nrow(coords)), each = 100))

    }

    else if (shape == "square") {

        sample_plots <- cbind(
            matrix(
                c(coords[, 1] - size, coords[, 1] - size,
                  coords[, 1] + size, coords[, 1] + size),
                ncol = 1),

            matrix(
                c(coords[, 2] - size, coords[, 2] + size,
                  coords[, 2] + size, coords[, 2] - size),
                ncol = 1),
            rep(seq_len(nrow(coords)), times = 4)
        )
    }

    else {

        stop(paste0("Shape option ", shape, " unkown."), call. = FALSE)

    }

    if (return_vec) {

        sample_plots <- split(sample_plots[, -3], sample_plots[, 3])

        sample_plots <- lapply(X = sample_plots, FUN = function(x) {

            mat <- matrix(x, ncol = 2)

            terra::vect(x = mat, type = "polygon")

        })

        sample_plots <- terra::vect(sample_plots)

    }

    return(sample_plots)
}

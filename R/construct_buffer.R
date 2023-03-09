#' construct_buffer
#'
#' @description Internal function to construct plot area around coordinates
#'
#' @param coords SpatialPoints or 2-column matrix with coordinates of sample points
#' @param shape String specifying plot shape. Either "circle" or "square"
#' @param size Size of sample plot. Equals the radius for circles or the
#' side-length for squares in mapunits
#' @param return_sf If true, sf objects are returned.
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
construct_buffer <- function(coords, shape, size, return_sf, verbose) UseMethod("construct_buffer")

#' @name construct_buffer
#' @export
construct_buffer.MULTIPOINT <- function(coords, shape, size,
                                        return_sf = TRUE, verbose = TRUE) {

    coords <- matrix(sf::st_coordinates(coords)[, 1:2], ncol = 2)

    construct_buffer_internal(coords, shape, size,
                              return_sf = return_sf, verbose = verbose)
}

#' @name construct_buffer
#' @export
construct_buffer.POINT <- function(coords, shape, size,
                                   return_sf = TRUE, verbose = TRUE) {

    coords <- matrix(sf::st_coordinates(coords)[, 1:2], ncol = 2)

    construct_buffer_internal(coords, shape, size,
                              return_sf = return_sf, verbose = verbose)
}

#' @name construct_buffer
#' @export
construct_buffer.sf <- function(coords, shape, size,
                                return_sf = TRUE, verbose = TRUE) {

    if (all(sf::st_geometry_type(coords) %in% c("POINT", "MULTIPOINT"))) {

        coords <- matrix(sf::st_coordinates(coords)[, 1:2], ncol = 2)

        construct_buffer_internal(coords, shape, size,
                                  return_sf = return_sf, verbose = verbose)
    }

    else{stop("Only POINT or MULTIPOINT features supported!!11!!1!!")}
}

#' @name construct_buffer
#' @export
construct_buffer.sfc <- function(coords, shape, size,
                                 return_sf = TRUE, verbose = TRUE) {

    if (all(sf::st_geometry_type(coords) %in% c("POINT", "MULTIPOINT"))) {

        coords <- matrix(sf::st_coordinates(coords)[, 1:2], ncol = 2)

        construct_buffer_internal(coords, shape, size,
                                  return_sf = return_sf, verbose = verbose)
    }

    else{stop("Only POINT or MULTIPOINT features supported!!11!!1!!")}
}

#' @name construct_buffer
#' @export
construct_buffer.matrix <- function(coords, shape , size, return_sf = TRUE, verbose = TRUE) {

    construct_buffer_internal(coords, shape, size,
                              return_sf = return_sf, verbose = verbose)
}

construct_buffer_internal <- function(coords, shape , size, return_sf = TRUE, verbose = TRUE) {

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
                c(coords[, 1] - size,
                  coords[, 1] - size,
                  coords[, 1] + size,
                  coords[, 1] + size),
                ncol = 1),

            matrix(
                c(coords[, 2] - size,
                  coords[, 2] + size,
                  coords[, 2] + size,
                  coords[, 2] - size),
                ncol = 1),
            rep(seq_len(nrow(coords)), times = 4)
        )
    }

    else{

        stop(paste0("Shape option ", shape, " unkown."), call. = FALSE)
    }

    if (return_sf) {

        sample_plots <- split(sample_plots[, -3], sample_plots[, 3])

        sample_plots <- lapply(X = sample_plots, FUN = function(x) {

            mat <- matrix(x, ncol = 2)

            sf::st_polygon(list(rbind(mat, mat[1, ])))

        })

        sample_plots <- sf::st_sf(data.frame(id = 1:nrow(coords), geom =  sf::st_sfc(sample_plots)))

    }

    return(sample_plots)
}

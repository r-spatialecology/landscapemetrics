#' CIRCLE (patch level)
#'
#' @description Related Circumscribing Circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE = 1 - (\frac{a_{ij}} {a_{ij}^{circle}})}
#' where \eqn{a_{ij}} is the area in square meters and \eqn{a_{ij}^{circle}} the area of
#' the smallest circumscribing circle.
#'
#' CIRCLE is a 'Shape metric'. The metric is the ratio between the patch area and the smallest
#' circumscribing circle of the patch. The diameter of the smallest circumscribing circle is
#' the 'diameter' of the patch connecting the opposing corner points of the two cells
#' that are the furthest away from each other. The metric characterises the compactness
#' of the patch and is comparable among patches with different area.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{0 <= CIRCLE < 1}
#' \subsection{Behaviour}{CIRCLE = 0 for a circular patch and approaches CIRCLE = 1 for
#' a linear patch.}
#'
#' @seealso
#' \code{\link{lsm_p_area}}, \cr
#' \code{\link{lsm_c_circle_mn}},
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_circle(landscape)
#'
#' @aliases lsm_p_circle
#' @rdname lsm_p_circle
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' @export
lsm_p_circle <- function(landscape, directions) UseMethod("lsm_p_circle")

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterLayer <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_circle_calc <- function(landscape, directions,
                              points = NULL, resolution = NULL) {

    # conver to matrix
    if (!methods::is(landscape, "matrix")) {

        # get coordinates and values of all cells
        points <- raster_to_points(landscape)[, 2:4]

        # get resolution
        resolution <- raster::res(landscape)

        # convert to matrix
        landscape <- raster::as.matrix(landscape)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "circle",
                              value = as.double(NA)))
    }

    # get resolution of landscape
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # patches with only 1 cell
    one_cell <- which(area_patch$value == prod(resolution) / 10000)

    # get unique classes
    classes <- get_unique_values(landscape)[[1]]

    circle_patch <- do.call(rbind,
                            lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions,
                                         return_raster = FALSE)[[1]]

        # only boundary cells need to be considered for circle diameter
        class_boundaries <- get_boundaries(landscape_labeled,
                                           directions = 4,
                                           as_NA = TRUE,
                                           consider_boundary = FALSE)[[1]]

        # transpose matrix to have same order as in points
        class_boundaries <- t(class_boundaries)
        landscape_labeled <- t(landscape_labeled)

        # get coordinates of current class
        points <- matrix(points[which(!is.na(class_boundaries)), ],
                         ncol = 3)

        # use patch id instead of class id
        points[, 3] <- landscape_labeled[!is.na(class_boundaries)]

        # get circle radius around patch
        circle <- rcpp_get_circle(points,
                                  resolution_x = resolution_x,
                                  resolution_y = resolution_y)
        # calculate circle area
        circle[, 2] <- pi * ((circle[, 2] / 2) ^ 2)

        # sort according to patch id
        circle <- matrix(circle[order(circle[,1]),], ncol = 2)

        tibble::tibble(class = patches_class,
                       value = circle[,2])
        })
    )

    # calculate circle metric
    circle_patch$value <- 1 - (area_patch$value * 10000) / circle_patch$value

    # set all one-cell patches to 0
    circle_patch$value[one_cell] <- 0

    tibble::tibble(
        level = "patch",
        class = as.integer(circle_patch$class),
        id = as.integer(seq_len(nrow(circle_patch))),
        metric = "circle",
        value = as.double(circle_patch$value)
    )
}

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

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterStack <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.RasterBrick <- function(landscape, directions = 8) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_circle
#' @export
lsm_p_circle.list <- function(landscape, directions = 8) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_p_circle_calc <- function(landscape, directions,
                              extent = NULL, resolution = NULL, crs = NULL) {

    # use raster instead of landscape
    if(class(landscape) == "matrix") {
        landscape <- matrix_to_raster(landscape,
                                      extent = extent,
                                      resolution = resolution,
                                      crs =crs)
    }

    # get resolution of landscape
    resolution <- raster::res(landscape)
    resolution_x <- resolution[[1]]
    resolution_y <- resolution[[2]]

    # get patch area
    area_patch <- lsm_p_area_calc(landscape, directions = directions)

    # patches with only 1 cell
    one_cell <- which(area_patch$value == prod(resolution) / 10000 )

    # get unique classes
    classes <- get_unique_values(landscape)[[1]]

    circle_patch <- lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches(landscape,
                                         class = patches_class,
                                         directions = directions)[[1]]

        landscape_boundaries <- raster::boundaries(landscape_labeled,
                                                   directions = 4,
                                                   asNA = TRUE)

        # convert to points
        points_class_labeled <- data.frame(raster::rasterToPoints(landscape_labeled))
        points_class_boundaries <- data.frame(raster::rasterToPoints(landscape_boundaries))

        # keep only points that are boundary (but with original patch id)
        points_class <- dplyr::semi_join(x = points_class_labeled,
                                         y = points_class_boundaries,
                                         by = c("x","y"))

        # get circle radius around patch
        circle <- rcpp_get_circle(as.matrix(points_class),
                                  resolution_x = resolution_x,
                                  resolution_y = resolution_y)
        # calculate circle area
        circle[, 2] <- pi * ((circle[, 2]  /2) ^ 2)

        # sort according to patch id
        circle <- matrix(circle[order(circle[,1]),], ncol = 2)

        tibble::tibble(class = patches_class,
                       value = circle[,2])
    })

    # calculate circle metric
    circle_patch <- dplyr::bind_rows(circle_patch)

    # calculate circle metric
    circle_patch <- dplyr::mutate(circle_patch,
                                  value = 1 - ((area_patch$value * 10000) / value))

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

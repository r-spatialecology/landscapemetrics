#' CIRCLE (patch level)
#'
#' @description Related Circumscribing Circle (Shape metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
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
#' lsm_p_circle(terra::unwrap(landscape))
#'
#' @aliases lsm_p_circle
#' @rdname lsm_p_circle
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' Baker, W. L., and Y. Cai. 1992. The r.le programs for multiscale analysis of
#' landscape structure using the GRASS geographical information system.
#' Landscape Ecology 7: 291-302.
#'
#' Based on C++ code from Project Nayuki (https://www.nayuki.io/page/smallest-enclosing-circle).
#'
#' @export
lsm_p_circle <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_circle_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_circle_calc <- function(landscape, directions, resolution = NULL) {

    # conver to matrix
    if (!inherits(x = landscape, what = "matrix")) {

        # get resolution
        resolution <- terra::res(landscape)

        # convert to matrix
        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # check if resolution is identical
    if (!isTRUE(all.equal(resolution[1], resolution[2]))) {

        stop("The area of the circumscribing circle is currently only implemented for equal resolutions.",
             call. = FALSE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "circle",
                              value = as.double(NA)))
    }

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # convert area to m2
    area_patch <- area_patch$value * 10000

    # get unique classes
    classes <- get_unique_values_int(landscape, verbose = FALSE)

    circle_patch <- do.call(rbind,
                            lapply(classes, function(patches_class) {

        # get connected patches
        landscape_labeled <- get_patches_int(landscape,
                                             class = patches_class,
                                             directions = directions)[[1]]

        # get circle radius around patch
        circle <- rcpp_get_circle(landscape_labeled,
                                  resolution_xy = resolution[[1]])

        tibble::tibble(class = patches_class,
                       value = circle$circle_area)
        })
    )

    # calculate circle metric
    circle_patch$value <- 1 - (area_patch / circle_patch$value)

    tibble::tibble(
        level = "patch",
        class = as.integer(circle_patch$class),
        id = as.integer(seq_len(nrow(circle_patch))),
        metric = "circle",
        value = as.double(circle_patch$value)
    )
}

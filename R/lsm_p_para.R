#' PARA (patch level)
#'
#' @description Perimeter-Area ratio (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick, stack, or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PARA = \frac{p_{ij}} {a_{ij}}}
#' where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
#' area in square meters.
#'
#' PARA is a 'Shape metric'. It describes the patch complexity in a
#' straightforward way. However, because it is not standarised to a certain
#' shape (e.g. a square), it is not scale independent, meaning that increasing
#' the patch size while not changing the patch form will change the ratio.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{PARA > 0}
#' \subsection{Behaviour}{Increases, without limit, as the shape complexity
#' increases.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_para_mn}},
#' \code{\link{lsm_c_para_sd}},
#' \code{\link{lsm_c_para_cv}}, \cr
#' \code{\link{lsm_l_para_mn}},
#' \code{\link{lsm_l_para_sd}},
#' \code{\link{lsm_l_para_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_para(terra::unwrap(landscape))
#'
#' @aliases lsm_p_para
#' @rdname lsm_p_para
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: https://www.umass.edu/landeco/
#'
#' @export
lsm_p_para <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_para_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_para_calc <- function(landscape, directions, resolution = NULL){

    # convert to matrix
    if (!inherits(x = landscape, what = "matrix")) {
        resolution <- terra::res(landscape)
        landscape <-terra::as.matrix(landscape, wide = TRUE)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::tibble(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "para",
                              value = as.double(NA)))
    }

    # get perim
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution)

    # get area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # calculate ratio between area and perim
    para_patch <- perimeter_patch$value / (area_patch$value * 10000)

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(perimeter_patch$id),
        metric = "para",
        value = as.double(para_patch)
    )
}

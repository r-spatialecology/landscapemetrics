#' PAFRAC  (landscape level)
#'
#' @description Perimeter-Area Fractal Dimension (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param verbose Print warning message if not sufficient patches are present
#'
#' @details
#' \deqn{PAFRAC = \frac{2}{\beta}}
#' where \eqn{\beta} is the slope of the regression of the area against the perimeter
#' (logarithm) \eqn{N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln a_{ij} = a + \beta N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln p_{ij}}
#'
#' PAFRAC is a 'Shape metric'. It describes the patch complexity of the landscape while being
#' scale independent. This means that increasing the patch size while not changing the
#' patch form will not change the metric. However, it is only meaningful if the relationship
#' between the area and perimeter is linear on a logarithmic scale. Furthermore, if there
#' are less than 10 patches in the landscape, the metric returns NA because of the small-sample
#' issue.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2}
#' \subsection{Behaviour}{Approaches PAFRAC = 1 for patches with simple shapes and
#' approaches PAFRAC = 2 for irregular shapes}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_p_perim}}, \cr
#' \code{\link{lsm_c_pafrac}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pafrac(landscape)
#'
#' @aliases lsm_l_pafrac
#' @rdname lsm_l_pafrac
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' Burrough, P. A. 1986. Principles of Geographical Information Systems for
#' Land Resources Assessment. Monographs on Soil and Resources Survey No. 12.
#' Clarendon Press, Oxford
#'
#' @export
lsm_l_pafrac <- function(landscape, directions, verbose) UseMethod("lsm_l_pafrac")

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterLayer <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterStack <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterBrick <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.stars <- function(landscape, directions = 8, verbose = TRUE) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_l_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.list <- function(landscape, directions = 8, verbose = TRUE) {

    result <- lapply(X = landscape,
                     FUN = lsm_l_pafrac_calc,
                     directions = directions,
                     verbose = verbose)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_l_pafrac_calc <- function(landscape, directions, verbose, resolution = NULL){

    # convert to matrix
    if(class(landscape) != "matrix") {
        resolution <- raster::res(landscape)
        landscape <- raster::as.matrix(landscape)
    }

    # get patch area
    area_patch <- lsm_p_area_calc(landscape,
                                  directions = directions,
                                  resolution = resolution)

    # get patch perimeter
    perimeter_patch <- lsm_p_perim_calc(landscape,
                                        directions = directions,
                                        resolution = resolution)

    # get number of patches for each class
    number_patches <- lsm_c_np_calc(landscape,
                                    directions = directions)

    # summarise for total landscape
    number_patches <- dplyr::summarise(number_patches, value = sum(value))

    # PAFRAC NA for less than 10 patches
    if(number_patches$value < 10){

        pafrac <-  NA

        if(isTRUE(verbose)){
            warning("PAFRAC = NA for NP < 10",
                    call. = FALSE)
        }
    }

    # calculate pafrac as regression between area and perimeter (beta)
    else{
        regression_model <- stats::lm(log(area_patch$value) ~
                                          log(perimeter_patch$value))
        pafrac <- 2 / regression_model$coefficients[[2]]
    }

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "pafrac",
        value = as.double(pafrac)
    )
}

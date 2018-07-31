#' PAFRAC  (landscape level)
#'
#' @description Perimeter-Area Fractal Dimension (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PAFRAC = \frac{2}{\beta}}
#' where \eqn{\beta} is the slope of the regression of the area against the perimeter
#' (logarithm) \eqn{N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln a_{ij} = a + \beta N \sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} \ln p_{ij}}
#'
#' PAFRAC is a 'Shape metric'. It describes the patch complexity of the landscape while beeing
#' scale independent. This means that increasing the patch size while not changing the
#' patch form will not change the metric. However, it is only meaningful if the relationship
#' between the area and perimeter is linear on a logarithmic scale. Furthermore, if there
#' are less than 10 patches in the landscape, the metric returns NA because of the small-sample
#' issue.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= PAFRAC <= 2}
#' \subsection{Behaviour}{Approaches PAFRAC = 1 for patches with simples shapes and
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
#' @export
lsm_l_pafrac <- function(landscape, directions) UseMethod("lsm_l_pafrac")

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterLayer <- function(landscape, directions= 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_pafrac_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterStack <- function(landscape, directions= 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_pafrac_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.RasterBrick <- function(landscape, directions= 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_pafrac_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_pafrac
#' @export
lsm_l_pafrac.list <- function(landscape, directions= 8) {
    purrr::map_dfr(landscape,
                   lsm_l_pafrac_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_pafrac_calc <- function(landscape, directions){

    area_patch <- lsm_p_area_calc(landscape, directions = directions)
    perimeter_patch <- lsm_p_perim_calc(landscape, directions = directions)

    np_landscape <- lsm_l_np_calc(landscape, directions = directions)

    if(np_landscape$value < 10){
        pafrac <-  NA
        warning("PAFRAC = NA for NP < 10",
                call. = FALSE)
    }

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

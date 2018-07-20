#' CIRCLE_MN (Class level)
#'
#' @description Mean of related circumscribing circle (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{CIRCLE_{MN} = mean(CIRCLE[patch_{ij}])}
#' where \eqn{CIRCLE[patch_{ij}]} is the related circumscribing circle of each patch.
#'
#' CIRCLE_MN is a 'Shape metric' and summarises each class as the mean of the related
#' circumscribing circle of all patches belonging to class i. CIRCLE describes
#' the ratio between the patch area and the smallest circumscribing circle of the patch
#' and characterises the compactness of the patch.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{CIRCLE_MN > 0}
#' \subsection{Behaviour}{Approaches CIRCLE_MN = 0 if the related circumscribing circle
#' of all patches is small. Increases, without limit, as the related circumscribing circles
#' increase.}
#'
#' @seealso
#' \code{\link{lsm_p_circle}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_circle_sd}},
#' \code{\link{lsm_c_circle_cv}}, \cr
#' \code{\link{lsm_l_circle_mn}},
#' \code{\link{lsm_l_circle_sd}},
#' \code{\link{lsm_l_circle_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_circle_mn(landscape)
#'
#' @aliases lsm_c_circle_mn
#' @rdname lsm_c_circle_mn
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_circle_mn <- function(landscape) UseMethod("lsm_c_circle_mn")

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_circle_mn
#' @export
lsm_c_circle_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_circle_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_circle_mn_calc <- function(landscape) {

    circle_mn  <- landscape %>%
        lsm_p_circle_calc() %>%
        dplyr::group_by(class)  %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(circle_mn$class),
        id = as.integer(NA),
        metric = "circle_mn",
        value = as.double(circle_mn$value)
    )

}


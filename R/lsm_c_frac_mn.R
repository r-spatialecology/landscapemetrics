#' FRAC_MN (class level)
#'
#' @description Mean fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{FRAC_{MN} = mean(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_MN is a 'Shape metric'. The metric summarises each class
#' as the mean of the fractal dimension index of all patches belonging to class i.
#' The fractal dimenstion index is based on the patch perimeter and
#' the patch area and describes the patch complexity. The coeffiecent of variation is
#' scaled to the mean and comparable among different landscapes.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_MN > 0 }
#' \subsection{Behaviour}{Approaches FRAC_MN = 1 if all patches are squared and FRAC_MN = 2
#'  if all patches are irregular.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link{mean}}, \cre
#' \code{\link{lsm_c_frac_sd}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @examples
#' lsm_c_frac_mn(landscape)
#'
#' @aliases lsm_c_frac_mn
#' @rdname lsm_c_frac_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_frac_mn <- function(landscape) UseMethod("lsm_c_frac_mn")

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_mn
#' @export
lsm_c_frac_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_frac_mn_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_frac_mn_calc <- function(landscape){

    frac_mean <- lsm_p_frac(landscape) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value))

    tibble::tibble(
        level = "patch",
        class = as.integer(frac_mean$class),
        id = as.integer(NA),
        metric = "fractal dimension index (mean)",
        value = as.double(frac_mean$value)
    )
}

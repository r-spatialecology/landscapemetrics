#' FRAC_SD (class level)
#'
#' @description Standard deviation fractal dimension index (Shape metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{FRAC_{SD} = sd(FRAC[patch_{ij}])}
#' where \eqn{FRAC[patch_{ij}]} equals the fractal dimension index of each patch.
#'
#' FRAC_SD is a 'Shape metric'. The metric summarises each class
#' as the standard deviation of the fractal dimension index of all patches
#' belonging to class i. The fractal dimenstion index is based on the patch perimeter and
#' the patch area and describes the patch complexity.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{FRAC_SD>= 0 }
#' \subsection{Behaviour}{Equals FRAC_SD = 0 if the fractal dimension index is identical
#' for all patches. Increases, without limit, as the variation of the fractal dimension
#' indices increases.}
#'
#' @seealso
#' \code{\link{lsm_p_frac}},
#' \code{\link{sd}}, \cre
#' \code{\link{lsm_c_frac_mn}},
#' \code{\link{lsm_c_frac_cv}}, \cr
#' \code{\link{lsm_l_frac_mn}},
#' \code{\link{lsm_l_frac_sd}},
#' \code{\link{lsm_l_frac_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_frac_sd(landscape)
#'
#' @aliases lsm_c_frac_sd
#' @rdname lsm_c_frac_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_frac_sd <- function(landscape) UseMethod("lsm_c_frac_sd")

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_frac_sd
#' @export
lsm_c_frac_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_frac_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_frac_sd_calc <- function(landscape){

    frac_sd <- landscape %>%
        lsm_p_frac_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "patch",
        class = as.integer(frac_sd$class),
        id = as.integer(NA),
        metric = "fractal dimension index (sd)",
        value = as.double(frac_sd$value)
    )
}

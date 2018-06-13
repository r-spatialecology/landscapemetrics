#'  Fractal dimension index  (patch level)
#'
#' @description Fractacl dimension index of patch (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' The fractal dimension index equals two times the natural logarithm of a
#' quarter of the patch area divided by the natural logarithm of the patch area
#' \deqn{FRAC = 2 * ln(0.25 * perimeter[patch]) / ln(area[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= FRAC <= 2 }
#' \subsection{Behaviour}{The fractal dimension index approaches FRAC = 1 for
#' simple patches and FRAC = 2 for irregular patches}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_frac(landscape)
#'
#' @aliases lsm_p_frac
#' @rdname lsm_p_frac
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_frac <- function(landscape) UseMethod("lsm_p_frac")

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_frac
#' @export
lsm_p_frac.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_frac_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_frac_calc <- function(landscape){

    perimeter <- lsm_p_perim(landscape)

    area <- lsm_p_area(landscape) %>%
        dplyr::mutate(value = value * 10000)

    frac <- 2 * log (0.25 * perimeter$value) / log(area$value)
    frac[is.na(frac)] <- 1

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "fractal dimension index",
        value = frac
    )
}

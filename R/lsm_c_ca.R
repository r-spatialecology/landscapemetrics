#' CA
#'
#' @description Total area of class i (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CA = sum(AREA[patch_{ij}])}
#' where \eqn{AREA[patch_{ij}]} is the area of each patch in hectares
#'
#' CA is an 'Area and egdge metric' and a measure of composition.
#' The total (class) area sums the area of all patches belonging to class i.
#' CA is an absolute measure, making comparisons among landscapes with different
#' total areas difficult.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CA > 0}
#' \subsection{Behaviour}{Aprroaches 0 as the patch areas of class i become small.
#' Increases, without limit, as the patch areas of class i become large. CA = TA if only
#' one class is present}
#'
#' seealso \code{\link{lsm_p_area}} and \code{\link{sum}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ca(landscape)
#'
#' @aliases lsm_c_ca
#' @rdname lsm_c_ca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_ca <- function(landscape) UseMethod("lsm_c_ca")

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ca
#' @export
lsm_c_ca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ca_calc <- function(landscape) {
    total_area <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = total_area$class,
        id = as.integer(NA),
        metric = "total area",
        value = total_area$value
    )
}

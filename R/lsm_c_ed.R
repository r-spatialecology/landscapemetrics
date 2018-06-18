#' Edge Density (class level)
#'
#' @description Edge Density (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary xxx
#'
#' @details
#' Edge density equals the sum of the length of all edges of class i divided by the total
#' area multiplied 10 000. Because edge density is relative, comparison among landcapes with different total areas
#' are possible
#' \deqn{ED = (sum(edges[class_i]) / total area) * 10 000}
#' \subsection{Units}{Meters per hectare}
#' \subsection{Range}{ED >= 0}
#' \subsection{Behaviour}{ED increases as the landcapes becomes more patchy}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ed(landscape)
#'
#' @aliases lsm_c_ed
#' @rdname lsm_c_ed
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ed <- function(landscape, count_boundary) UseMethod("lsm_c_ed")

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterLayer <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterStack <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.RasterBrick <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ed
#' @export
lsm_c_ed.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_ed_calc <- function(landscape, count_boundary) {

    area_landscape <- lsm_l_ta_calc(landscape)

    ed <- landscape %>%
        lsm_c_te_calc(count_boundary = count_boundary) %>%
        dplyr::mutate(value = value / area_landscape$value)

    tibble::tibble(
        level = "class",
        class = ed$class,
        id = as.integer(NA),
        metric = "edge density",
        value = ed$value
    )
}

#' Patch Cohesion Index (class level)
#'
#' @description Patch Cohesion Index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' ???
#'
#' \deqn{???}
#' \subsection{Units}{???}
#' \subsection{Ranges}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_cohesion(landscape)
#'
#' @aliases lsm_c_cohesion
#' @rdname lsm_c_cohesion
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_cohesion <- function(landscape)
    UseMethod("lsm_c_cohesion")

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_cohesion
#' @export
lsm_c_cohesion.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_cohesion_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_cohesion_calc <- function(landscape) {

    ncells_landscape <- landscape %>%
        lsm_l_ta_calc() %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape)))

    ncells_patch <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value * 10000 / prod(raster::res(landscape))) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

   cohesion <- landscape %>%
        lsm_p_perim_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(
            value = ((1 - (value / (value * sqrt(ncells_patch$value)))) *
                ((1 - (1 / sqrt(ncells_landscape$value))) ^ - 1)) * 100
        )

   tibble::tibble(
       level = "class",
       class = cohesion$class,
       id = as.integer(NA),
       metric = "patch cohesion index",
       value = cohesion$value
   )
}

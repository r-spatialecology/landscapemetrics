#' Area  (patch level)
#'
#' @description Area of a patch (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Equals the area of the patch
#' \subsection{Units}{hectares}
#' \subsection{Range}{AREA > 0}
#' \subsection{Behaviour}{Increases without limit as the patch size increases}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_area(landscape)
#'
#' @aliases lsm_p_area
#' @rdname lsm_p_area
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_area <- function(landscape) UseMethod("lsm_p_area")

#' @name lsm_p_area
#' @export
lsm_p_area.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_area_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_area
#' @export
lsm_p_area.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_area_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_area
#' @export
lsm_p_area.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_area_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_area
#' @export
lsm_p_area.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_area_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_area_calc <- function(landscape){
    labeled_landscape <- landscape %>%
        cclabel()

    area_class <- labeled_landscape %>%
        unname() %>%
        purrr::map_dfr(function(patches_class){
            area_patch <- patches_class %>%
                raster::values() %>%
                table(useNA = "no") %>%
                magrittr::multiply_by(prod(raster::res(patches_class))) %>%
                magrittr::divide_by(10000)

            tibble::tibble(
                id = NA,
                value = area_patch
            )
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(area_class$class),
        id = seq_len(nrow(area_class)),
        metric = "area",
        value = area_class$value
    )
}



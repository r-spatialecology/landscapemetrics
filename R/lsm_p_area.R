#' Patch area  (patch level)
#'
#' @description Patch size (patch level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_area(landscape)
#' lsm_p_area(landscape_stack)
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

# Not working yet!
lsm_p_area_calc <- function(landscape){
    area <- landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(x){
            tibble(
                area = raster::values(x) %>%
                    table(useNA = "no")  %>%
                    magrittr::multiply_by(prod(raster::res(landscape)))
            )
        }) %>%
        dplyr::pull(area)

    tibble::tibble(
        level = "patch",
        id = seq_along(area),
        metric = "patch area (mean)",
        value = area
    )
}



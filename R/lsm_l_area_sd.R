#' Patch area distribution (landscape level)
#'
#' @description Standard deviation patch size (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_area_sd(landscape)
#' lsm_l_area_sd(landscape_stack)
#'
#' @aliases lsm_l_area_sd
#' @rdname lsm_l_area_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_area_sd <- function(landscape) UseMethod("lsm_l_area_sd")

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_area_sd
#' @export
lsm_l_area_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_area_sd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

# Not working yet!
lsm_l_area_sd_calc <- function(landscape){
    area_sd <- landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(x){
            tibble::tibble(
                area = raster::values(x) %>%
                    table(useNA = "no")  %>%
                    magrittr::multiply_by(prod(raster::res(landscape)))
            )
        }) %>%
        dplyr::pull(area) %>%
        stats::sd()

    tibble::tibble(
        level = "landscape",
        id = as.integer(NA),
        metric = "patch area (sd)",
        value = area_sd
    )
}



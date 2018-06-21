#' Patch density (landscape level)
#'
#' @description Patch density (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Patch density equals the number of all patches in the landscape divided by the total
#' area. Patch density is a relative measure and compareable among landscapes with
#' different total areas
#' \deqn{PD = number of patches / total area}
#' \subsection{Units}{Number per hectares (Number per 100 ha in FRAGSTATS???)}
#' \subsection{Ranges}{PD > 0}
#' \subsection{Behaviour}{Increases as the landscape gets more patchy}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_pd(landscape)
#'
#' @aliases lsm_l_pd
#' @rdname lsm_l_pd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_l_pd <- function(landscape) UseMethod("lsm_l_pd")

#' @name lsm_l_pd
#' @export
lsm_l_pd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pd
#' @export
lsm_l_pd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pd
#' @export
lsm_l_pd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_pd
#' @export
lsm_l_pd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_pd_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_pd_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    patch_density <- landscape %>%
        lsm_l_np_calc() %>%
        dplyr::mutate(value = (value / area_landscape$value) * 100)

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "patch density",
        value = patch_density$value
    )
}

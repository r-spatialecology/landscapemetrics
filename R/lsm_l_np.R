#' Number of patches (landscape level)
#'
#' @description Number of patches (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Number of patches equals the number of patches in the landscape
#' \subsection{Units}{None}
#' \subsection{Ranges}{NP >= 1}
#' \subsection{Behaviour}{NP = 1 when only one class and patch is present and
#' increases without limit as the number of patches increases}
#' @return tibble
#'
#' @examples
#' lsm_l_np(landscape)
#'
#' @aliases lsm_l_np
#' @rdname lsm_l_np
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_np <- function(landscape) UseMethod("lsm_l_np")

#' @name lsm_l_np
#' @export
lsm_l_np.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_np_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterStack <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.RasterBrick <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_np
#' @export
lsm_l_np.list <- function(landscape) {
    purrr::map_dfr(
        raster::as.list(landscape),
        .f = lsm_l_np_calc,
        .id = "layer"
    ) %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_np_calc <- function(landscape) {

    n_patches <- landscape %>%
        lsm_c_np() %>%
        dplyr::summarise(value = sum(value))


    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "number of patches",
        value = n_patches$value
    )

}

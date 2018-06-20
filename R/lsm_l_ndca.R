#' Number of core areas (landscape level)
#'
#' @description Number of core areas (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Number of core areas equals the sum of number of core areas of all patches in the
#' landscape. Called number of disjunct core areas in FRAGSTATS. A core area is a
#' 'patch within the patch' without any edge cells. In other words, the number of
#' patches within the patch that only have neighbouring cells of the same type
#' \deqn{ndca = sum(ndca[patch])}
#' \subsection{Units}{None}
#' \subsection{Range}{ndca >= 0}
#' \subsection{Behaviour}{ndca = 0 when CORE = 0, i.e. every cell in patches of class i is
#' an edge. ndca increases without limit as core areas become more present, i.e. patches
#' becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_ndca(landscape)
#'
#' @aliases lsm_l_ndca
#' @rdname lsm_l_ndca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_ndca <- function(landscape) UseMethod("lsm_l_ndca")

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_ndca
#' @export
lsm_l_ndca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_l_ndca_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_ndca_calc <- function(landscape){

    ndca <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "landscape",
        class =  as.integer(NA),
        id = as.integer(NA),
        metric = "number of disjunct core areas",
        value = ndca$value
    )
}

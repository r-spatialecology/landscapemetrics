#' Number of core areas (class level)
#'
#' @description Number of core areas (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' Number of core areas equals the sum of number of core areas of class i.
#' Called number of disjunct core areas in FRAGSTATS. A core area is a 'patch within
#' the patch' without any edge cells. In other words, the number of patches within
#' the patch that only have neighbouring cells of the same type
#' \deqn{DCORE = sum(NCORE[patch_i])}
#' \subsection{Units}{None}
#' \subsection{Range}{DCAD >= 0}
#' \subsection{Behaviour}{DCAD = 0 when CORE = 0, i.e. every cell in patches of class i is
#' an edge. DCAD increases without limit as core areas become more present, i.e. patches
#' becoming larger and less complex}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ndca(landscape)
#'
#' @aliases lsm_c_ndca
#' @rdname lsm_c_ndca
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ndca <- function(landscape) UseMethod("lsm_c_ndca")

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ndca_calc <- function(landscape){
    dcad <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(dcad$class),
        id = as.integer(NA),
        metric = "number of core areas",
        value = as.double(dcad$value)
    )
}

#' CORE_MN (class level)
#'
#' @description Mean of core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{CORE_{MN} = mean(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_MN is a 'Core area metric' and equals the mean of core areas of all patches
#' belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_MN >= 0}
#' \subsection{Behaviour}{Equals CORE_MN = 0 if CORE = 0 for all patches. Increases,
#' without limit, as the corea area indices increase.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{mean}}, \cr
#' \code{\link{lsm_c_sd}},
#' \code{\link{lsm_c_cv}}, \cr
#' \code{\link{lsm_l_mn}},
#' \code{\link{lsm_l_sd}},
#' \code{\link{lsm_l_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_mn(landscape)
#'
#' @aliases lsm_c_core_mn
#' @rdname lsm_c_core_mn
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core_mn <- function(landscape) UseMethod("lsm_c_core_mn")

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_mn
#' @export
lsm_c_core_mn.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_core_mn_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_core_mn_calc <- function(landscape){

    core_mean <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = mean(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(core_mean$class),
        id = as.integer(NA),
        metric = "core area (mean)",
        value = as.double(core_mean$value)
    )
}

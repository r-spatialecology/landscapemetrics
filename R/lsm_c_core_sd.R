#' Core area distribution (class level)
#'
#' @description Standard deviation of patch core area (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{CORE_{SD} = sd(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_SD is a 'Core area metric'. It equals the standard deviation of the core area
#' of each patch belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among patches of the same class i in the landscape.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_SD >= 0}
#' \subsection{Behaviour}{Equals CORE_SD = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{sd}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_cv}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_sd(landscape)
#'
#' @aliases lsm_c_core_sd
#' @rdname lsm_c_core_sd
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core_sd <- function(landscape) UseMethod("lsm_c_core_sd")

#' @name lsm_c_core_sd
#' @export
lsm_c_core_sd.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_sd_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_sd
#' @export
lsm_c_core_sd.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_sd_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_sd
#' @export
lsm_c_core_sd.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_sd_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_sd
#' @export
lsm_c_core_sd.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_core_sd_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_core_sd_calc <- function(landscape){

    core_sd <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = stats::sd(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(core_sd$class),
        id = as.integer(NA),
        metric = "core area (sd)",
        value = as.double(core_sd$value)
    )
}

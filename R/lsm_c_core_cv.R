#' CORE_CV (class level)
#'
#' @description Coeffiecent of variation of core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CORE_{CV} = cv(CORE[patch_{ij}])}
#' where \eqn{CORE[patch_{ij}]} is the core area in square meters of each patch.
#'
#' CORE_CV is a 'Core area metric'. It equals the coeffiecent of variation of the core area
#' of each patch belonging to class i. The core area is defined as all cells that have no
#' neighbour with a different value than themselves (rook's case). The metric describes the
#' differences among patches of the same class i in the landscape and is easily comparable
#' because it is scaled to the mean.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE_CV >= 0}
#' \subsection{Behaviour}{Equals CORE_CV = 0 if all patches have the same core area.
#' Increases, without limit, as the variation of patch core areas increases.}
#'
#' @seealso
#' \code{\link{lsm_p_core}},
#' \code{\link{cv}}, \cr
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_core_cv(landscape)
#'
#' @aliases lsm_c_core_cv
#' @rdname lsm_c_core_cv
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_core_cv <- function(landscape) UseMethod("lsm_c_core_cv")

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_core_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_core_cv
#' @export
lsm_c_core_cv.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_core_cv_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_core_cv_calc <- function(landscape){

    core_cv <- landscape %>%
        lsm_p_core_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = raster::cv(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(core_cv$class),
        id = as.integer(NA),
        metric = "core area (cv)",
        value = as.double(core_cv$value)
    )
}

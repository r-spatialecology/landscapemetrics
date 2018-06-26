#' CORE (patch level)
#'
#' @description Core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CORE = a_{ij}^{core}}
#' where \eqn{a_{ij}^{core}} is the core area in square meters
#'
#' CORE is a 'Core area metric' and equals the area within a patch that is not
#' on the edge of it. A cell is defined as core area if the cell has no
#' neighbour with a different value than itself (rook's case). It describes patch area
#' and shape simultaneously (more core area when the patch is large and the shape is
#' rather compact, i.e. a square).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE >= 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch area increases
#' and the patch shape simplifies (more core area). CORE = 0 when every cell in
#' the patch is an edge.}
#'
#' @seealso
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}},
#' \code{\link{lsm_c_tca}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}},
#' \code{\link{lsm_l_tca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#'
#' @examples
#' lsm_p_core(landscape)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_core <- function(landscape) UseMethod("lsm_p_core")

#' @name lsm_p_core
#' @export
lsm_p_core.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc,.id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_core_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_core_calc <- function(landscape){

    core_area_patch <- landscape %>%
        cclabel() %>%
        purrr::map_dfr(function(patches_class) {
            patches_class %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_id) {
                    patches_class[patches_class != patch_id |
                                        is.na(patches_class)] <- -999
                    core_cells <- raster::Which(patches_class == patch_id, cells = T) %>%
                        purrr::map_dbl(function(cell_id) {
                            adjacent_cells <- raster::adjacent(patches_class,
                                                               cells = cell_id,
                                                               directions = 4,
                                                               pairs = FALSE)
                            ifelse(all(patches_class[adjacent_cells] == patch_id), cell_id, NA)
                        }) %>%
                        na.omit() %>%
                        length()

                    class_name <- patches_class %>%
                        names() %>%
                        sub("Class_", "", .)

                    tibble::tibble(
                        class = class_name,
                        value = (core_cells * prod(raster::res(landscape)) / 10000)
                    )
                })
        })

    tibble::tibble(
        level = "patch",
        class = as.integer(core_area_patch$class),
        id = as.integer(seq_len(nrow(core_area_patch))),
        metric = "core area",
        value = as.double(core_area_patch$value)
    )
}

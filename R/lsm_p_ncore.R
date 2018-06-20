#' NCORE (patch level)
#'
#' @description Number of core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' #' @details
#' \deqn{NCORE = n_{ij}^{core}}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas.
#'
#' NCORE is a 'Core and area metric'. A cell is defined as core if the cell has no
#' neighbour with a different value than itself (rook's case). The metric counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#' It describes patch area and shape simultaneously (more core area when the patch is large,
#' however, the shape must allow disjunct core areas). Thereby, a compact shape (e.g. a square)
#' will contain less disjunct core areas than a more irregular patch.
#
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patch is edge.
#' Increases, without limit, as core area increases and patch shape allows disjunct core
#' areas (i.e. patch shape becomes rather complex).}
#'
#' @seealso
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}},
#' \code{\link{lsm_c_ndca}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}},
#' \code{\link{lsm_l_ndca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#' @importFrom raster ncell
#'
#' @examples
#' lsm_p_ncore(landscape)
#'
#' @aliases lsm_p_ncore
#' @rdname lsm_p_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_ncore <- function(landscape) UseMethod("lsm_p_ncore")

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_ncore_calc <- function(landscape){

    core_class <- landscape %>%
        cclabel() %>%
        unname() %>%
        purrr::map_dfr(function(landscape_patch) {
            landscape_patch %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_id) {
                    landscape_patch[landscape_patch != patch_id |
                                        is.na(landscape_patch)] <- -999
                    core_cells <- raster::Which(landscape_patch == patch_id, cells = T) %>%
                        purrr::map_dbl(function(cell_id) {
                        adjacent_cells <- raster::adjacent(landscape_patch,
                                                           cells = cell_id,
                                                           directions = 4,
                                                           pairs = FALSE)
                        ifelse(all(landscape_patch[adjacent_cells] == patch_id), cell_id, NA)
                        }) %>%
                        na.omit()

                    if(length(core_cells) != 0){
                        landscape_patch[!1:ncell(landscape_patch) %in% core_cells] <- NA

                        core_patch_n <- landscape_patch %>%
                            lsm_l_np() %>%
                            dplyr::pull(value)
                    }
                    else{
                        core_patch_n <- 0
                    }

                    tibble::tibble(
                        id = NA,
                        value = core_patch_n
                    )
                })
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = as.integer(seq_len(nrow(core_class))),
        metric = "number of core areas",
        value = as.double(core_class$value)
    )
}

#'  Number of core areas  (patch level)
#'
#' @description Number of core cells
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_ncore(landscape)
#' lsm_p_ncore(landscape_stack)
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
        # padding(padding_value = NA) %>% Nedded ?
        cclabel() %>%
        unname() %>%
        purrr::map_dfr(function(x) {

            x %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(y) {
                    x[x != y | is.na(x)] <- -999

                    core_patch_n <- raster::Which(x == y, cells = T) %>%
                        purrr::map_dbl(function(z) {
                        adjacent_cells <- raster::adjacent(x,
                                                           cells = z,
                                                           directions = 4, # Use matrix for direction
                                                           pairs = FALSE)

                        all(x[adjacent_cells] == y)
                        }) %>%
                        sum()

                    tibble::tibble(
                        id = NA,
                        value = core_patch_n
                    )
                })
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = seq_len(nrow(core_class)),
        metric = "number of core area",
        value = core_class$value
    )
}

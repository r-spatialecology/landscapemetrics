#'  Perimeter  (patch level)
#'
#' @description Perimeter of patch
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_perim(landscape)
#' lsm_p_perim(landscape_stack)
#'
#' @aliases lsm_p_perim
#' @rdname lsm_p_perim
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_perim <- function(landscape) UseMethod("lsm_p_perim")

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_perim_calc <- function(landscape){

    perimeter_class <- landscape %>%
        padding(padding_value = NA) %>%
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

                    adjacent_cells <- raster::adjacent(x = x,
                                                       cells = seq_len(raster::ncell(x)),
                                                       directions = 4,
                                                       pairs = TRUE)

                    neighbour_matrix <- table(x[adjacent_cells[,1]],
                                              x[adjacent_cells[,2]])

                    perimeter_patch_n <- neighbour_matrix[2:ncol(neighbour_matrix),1] *
                        prod(raster::res(x))

                    tibble::tibble(
                        id = NA,
                        value = perimeter_patch_n
                    )
                })
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_class$class),
        id = seq_len(nrow(perimeter_class)),
        metric = "perimeter",
        value = perimeter_class$value
    )
}

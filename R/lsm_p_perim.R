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
    labeled_landscape <- landscape %>%
        cclabel()

    perimeter <- labeled_landscape %>%
        seq_along() %>%
        purrr::map_dfr(function(x) {

            landscape_patch <- padding(labeled_landscape[[x]])

            patches_class <- labeled_landscape[[x]] %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique()

            class_id <- x

            patches_class %>%
                seq_along() %>%
                purrr::map_dfr(function(x) {
                    landscape_patch[landscape_patch != x | is.na(landscape_patch)] <- -999
                    adjacent_cells <- raster::adjacent(landscape_patch,
                                                       seq_len(raster::ncell(landscape_patch)),
                                                       4,
                                                       pairs=TRUE)

                    neighbour_matrix <- table(landscape_patch[adjacent_cells[,1]],
                                              landscape_patch[adjacent_cells[,2]])

                    perimeter <- neighbour_matrix[2:ncol(neighbour_matrix),1] *
                        prod(raster::res(landscape_patch))

                    tibble::tibble(
                        class = class_id,
                        id = x,
                        value = perimeter
                    )
                })
        })

    tibble::tibble(
        level = "patch",
        class = perimeter$class,
        id = perimeter$id,
        metric = "perimeter",
        value = perimeter$value
    )
}

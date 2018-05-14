#' Edge Density (ED, class scale)
#'
#' @description Edge Density
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param class Which class
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ed(landscape, 1)
#' lsm_c_ed(landscape_stack, 1)
#' @aliases lsm_c_ed
#' @rdname lsm_c_ed
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ed <- function(landscape, class) UseMethod("lsm_c_ed")

#' @name lsm_c_pland
#' @export
lsm_c_ed.RasterLayer <- function(landscape, class) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   class = class,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pland
#' @export
lsm_c_ed.RasterStack <- function(landscape, class) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   class = class,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.RasterBrick <- function(landscape, class) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   class = class,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pland
#' @export
lsm_c_pland.list <- function(landscape, class) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_ed_calc,
                   class = class,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}



############## WIP ############################################################
lsm_c_ed_calc <- function(landscape,
                          class){

    # cclabel class
    cclabeled_raster <- cclabel(landscape, class)[[1]]

    # set background to calculate number of neighbors next to cells with
    # values of -999
    cclabeled_raster[is.na(cclabeled_raster)] <- -999

    # compute neighborhood matrix
    adjacent_cells <- raster::adjacent(cclabeled_raster,
                                       seq_len(raster::ncell(cclabeled_raster)),
                                       4,
                                       pairs=TRUE)
    # count whos neighbor of who
    tb <- table(cclabeled_raster[adjacent_cells[,1]],
                cclabeled_raster[adjacent_cells[,2]])

    # return first row with counts of adjents sites between patches and
    # cells with -999
    total_edge <- tibble::tibble(
        level = 'landscape',
        id = as.numeric(NA),
        metric = 'total edge',
        value = (sum(tb[2:ncol(tb),1])/2) * prod(raster::res(landscape))
    )

    return(total_edge)
}

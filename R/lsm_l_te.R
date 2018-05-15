#'  Total Edge (TE, landscape scale)
#'
#' @description  Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_l_te(landscape)
#' lsm_l_te(landscape_stack)
#'
#' @aliases lsm_l_te
#' @rdname lsm_l_te
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_l_te <- function(landscape) UseMethod("lsm_l_te")

#' @name lsm_l_te
#' @export
lsm_l_te.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_te_calc <- function(landscape){

    # calculate total edge length by mapping over every class
    total_edge <- purrr::map(raster::unique(landscape), function(x) {

        # cclabel class
        cclabeled_raster <- cclabel(landscape, x)[[1]]

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
        tb[2:ncol(tb),1]

    })

    total_edge <- tibble::tibble(
        level = 'landscape',
        id = as.integer(NA),
        metric = 'total edge',
        value = (sum(unlist(total_edge))/2) * prod(raster::res(landscape))
    )

    return(total_edge)

}

#' Total Edge  (TE, class scale)
#'
#' @description Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_c_te(landscape)
#' lsm_c_te(landscape_stack)
#' lsm_c_te(raster::as.list(landscape_stack))
#'
#' @aliases lsm_c_te
#' @rdname lsm_c_te
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_te <- function(landscape) UseMethod("lsm_c_te")

#' @name lsm_c_te
#' @export
lsm_c_te.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_te_calc <- function(landscape) {
    # cclabel class
    cclabeled_raster <- cclabel(landscape)

    # set background to calculate number of neighbors next to cells with
    # values of -999
    te <- purrr::map_dbl(seq_along(cclabeled_raster),
                   function(x){
                       cclabeled_raster[[x]][is.na(cclabeled_raster[[x]])] <- -999

                       # compute neighborhood matrix
                       adjacent_cells <- raster::adjacent(cclabeled_raster[[x]],
                                                          seq_len(raster::ncell(cclabeled_raster[[x]])),
                                                          4,
                                                          pairs=TRUE)
                       # count whos neighbor of who
                       tb <- table(cclabeled_raster[[x]][adjacent_cells[,1]],
                                   cclabeled_raster[[x]][adjacent_cells[,2]])
                       te <- (sum(tb[2:ncol(tb),1])/2) * prod(raster::res(landscape))
                   })

    # return first row with counts of adjents sites between patches and
    # cells with -999
    tibble::tibble(
        level = "class",
        class = raster::unique(landscape),
        id = as.integer(NA),
        metric = "percentage of landscape",
        value = te
    )
}

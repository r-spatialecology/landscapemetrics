#' Total Edge (TE, landscape scale)
#'
#' @description  Total Edge
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Include landscape boundary in edge length

#' @return tibble
#'
#' @examples
#' lsm_l_te(landscape)
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
lsm_l_te <- function(landscape, count_boundary) UseMethod("lsm_l_te")

#' @name lsm_l_te
#' @export
lsm_l_te.RasterLayer <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   count_boundary = count_boundary,
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterStack <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   count_boundary = count_boundary,
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterBrick <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   count_boundary = count_boundary,
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_l_te
#' @export
lsm_l_te.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   count_boundary = count_boundary,
                   .f = lsm_l_te_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_l_te_calc <- function(landscape, count_boundary){

    if(isTRUE(count_boundary)){
        landscape <- padding(landscape = landscape,
                             padding_value = max(raster::values(landscape)) + 1,
                             padding_cells = 1)
    }



    adjacent_cells <- raster::adjacent(landscape,
                                       seq_len(raster::ncell(landscape)),
                                       4,
                                       pairs=TRUE)
    # count whos neighbor of who
    tb <- table(landscape[adjacent_cells[,1]],
                landscape[adjacent_cells[,2]])
    te <- sum(tb[lower.tri(tb)]) * raster::res(landscape)[[1]]

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "total edge",
        value = te
    )

}

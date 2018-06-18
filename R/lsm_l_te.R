#' Total Edge  (landscape level)
#'
#' @description Total edge of class (landscape level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param ... Specific arguments for certain functions, if not provided they fall back to default.
#'
#' @details
#' Total edge equals the sum of the length of all edges in the landscape.
#' Total edge is an absolute measure making comparisons among landscapes with
#' different total areas difficult. ??? Landscape border included or not ???
#' \deqn{TE = sum(edges)}
#' \subsection{Units}{Meters}
#' \subsection{Range}{TE >= 0}
#' \subsection{Behaviour}{TE increases without limit as landscape becomes more patchy}
#'
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
lsm_l_te <- function(landscape,  ...) UseMethod("lsm_l_te")

#' @name lsm_l_te
#' @export
lsm_l_te.RasterLayer <- function(landscape,  ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterStack <- function(landscape,  ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.RasterBrick <- function(landscape,  ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_te
#' @export
lsm_l_te.list <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_l_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_te_calc <- function(landscape, count_boundary = FALSE){

    if(isTRUE(count_boundary)){
        landscape <- pad_raster(landscape = landscape,
                             pad_raster_value = max(raster::values(landscape)) + 1,
                             pad_raster_cells = 1)
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

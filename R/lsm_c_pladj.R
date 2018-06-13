#' Percentage of Like Adjacencies (class level)
#'
#' @description Percentage of Like Adjacencies (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' Percentage of like adjacencies equals the number of like adjacencies of
#' class i divided by the total number of like adjacencies. In other other words,
#' PLADJ equals the percentage of like adjacencies of class i of all like
#' adjacencies. It is a measure of class aggregation.
#'
#' \deqn{PLADJ = Number of like adjacencies / sum(Number of like adjacencies)}
#' \subsection{Units}{Percentage}
#' \subsection{Ranges}{0 <= PLADJ <= 100}
#' \subsection{Behaviour}{PLADJ equals PLADJ = 0 when class i is maximally dissagregated,
#' i.e. every cell is a different patch. PLADJ = 100 when the only one class and patch
#' is present}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_pladj(landscape)
#'
#' @aliases lsm_c_pladj
#' @rdname lsm_c_pladj
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_pladj <- function(landscape)
    UseMethod("lsm_c_pladj")

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_pladj_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_pladj_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_pladj_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_pladj
#' @export
lsm_c_pladj.list <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_pladj_calc,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_pladj_calc <- function(landscape) {
    adjacent_cells <- raster::adjacent(
        landscape,
        cells =  seq_len(raster::ncell(landscape)),
        directions = 4,
        pairs = TRUE
    )

    tb <- table(landscape[adjacent_cells[, 1]],
                landscape[adjacent_cells[, 2]])

    pladj <- purrr::map_dbl(seq_len(nrow(tb)), function(x) {
        like_adjacencies <- tb[x, x]
        total_adjacencies <- sum(tb[x, ])

        like_adjacencies / total_adjacencies * 100
    })

    tibble::tibble(
        level = "class",
        class = raster::unique(landscape),
        id = as.integer(NA),
        metric = "percentage of like adjacencies",
        value = pladj
    )
}

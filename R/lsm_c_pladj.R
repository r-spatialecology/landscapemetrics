#' PLADJ (class level)
#'
#' @description Percentage of Like Adjacencies (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PLADJ = (\frac{g_{ij}}{\sum_{k=1}^{m} g_{ik}}) * 100}
#' where \eqn{g_{ii}} is the number of adjacencies between cells of class i
#' and \eqn{g_{ik}} is the number of adjacencies between cells of class i and k.
#'
#' PLADJ is an 'Aggregation metric'. It calculates the frequency how often patches of
#' different classes i (focal class) and k are next to eacht other, and following is a
#' measure of class aggregation. The adjacencies are counted using the double-count method.
#'
#' \subsection{Units}{Percent}
#' \subsection{Ranges}{0 <= PLADJ <= 100}
#' \subsection{Behaviour}{Equals PLADJ = 0 if class i is maximal dissagregated,
#' i.e. every cell is a different patch. Equals PLADJ = 100 when the only one patch
#' is present.}
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
        class = as.integer(raster::unique(landscape)),
        id = as.integer(NA),
        metric = "percentage of like adjacencies",
        value = as.double(pladj)
    )
}

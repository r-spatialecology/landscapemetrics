#' Total Edge  (class level)
#'
#' @description Total edge of class (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Include landscape boundary in edge length
#'
#' @details
#' Total edge equals the sum of the length of all edges of class i. Total edge is an
#' absolute measure making comparisons among landscapes with different total areas difficult.
#' ??? Landscape border included or not ???
#' \deqn{TE = sum(edges[class_i])}
#' \subsection{Units}{Meters}
#' \subsection{Range}{TE >= 0}
#' \subsection{Behaviour}{TE increases without limit as landscape becomes more patchy}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_te(landscape)
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
lsm_c_te <- function(landscape, ...) UseMethod("lsm_c_te")

#' @name lsm_c_te
#' @export
lsm_c_te.RasterLayer <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterStack <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterBrick <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   ....,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.list <- function(landscape, ...) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   ...,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_te_calc <- function(landscape, count_boundary = FALSE) {
    # cclabel class
    cclabeled_raster <- cclabel(landscape)

    # set background to calculate number of neighbors next to cells with
    # values of -999
    te <- purrr::map_dbl(seq_along(cclabeled_raster),
                   function(x){
                       cclabeled_raster[[x]][is.na(cclabeled_raster[[x]])] <- -999

                       if(isTRUE(count_boundary)){
                       cclabeled_raster[[x]] <- pad_raster(landscape = cclabeled_raster[[x]],
                                                        pad_raster_value = -999,
                                                        pad_raster_cells = 1)
                       }

                       # compute neighborhood matrix
                       adjacent_cells <- raster::adjacent(cclabeled_raster[[x]],
                                                          seq_len(raster::ncell(cclabeled_raster[[x]])),
                                                          4,
                                                          pairs=TRUE)
                       # count whos neighbor of who
                       tb <- table(cclabeled_raster[[x]][adjacent_cells[,1]],
                                   cclabeled_raster[[x]][adjacent_cells[,2]])
                       te <- (sum(tb[2:ncol(tb),1])) * raster::res(landscape)[[1]]
                   })

    # return first row with counts of adjents sites between patches and
    # cells with -999
    tibble::tibble(
        level = "class",
        class = raster::unique(landscape),
        id = as.integer(NA),
        metric = "total edge",
        value = te
    )
}

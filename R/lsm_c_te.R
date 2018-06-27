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
lsm_c_te <- function(landscape, count_boundary) UseMethod("lsm_c_te")

#' @name lsm_c_te
#' @export
lsm_c_te.RasterLayer <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterStack <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.RasterBrick <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_te
#' @export
lsm_c_te.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_te_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_c_te_calc <- function(landscape, count_boundary = FALSE) {

    cclabeled_raster <- cclabel(landscape)

    cclabeled_raster %>%
        purrr::map_dfr(function(patches_class){

            class_name <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            patches_class[is.na(patches_class)] <- -999

            if(isTRUE(count_boundary)){
                patches_class <- pad_raster(landscape = patches_class,
                                            pad_raster_value = -999,
                                            pad_raster_cells = 1)
            }

            adjacent_cells <- raster::adjacent(patches_class,
                                               seq_len(raster::ncell(patches_class)),
                                               directions = 4,
                                               pairs=TRUE)

            tb <- table(patches_class[adjacent_cells[,1]],
                        patches_class[adjacent_cells[,2]])
            te <- (sum(tb[2:ncol(tb),1])) * raster::res(landscape)[[1]]

            tibble::tibble(
                level = "class",
                class = as.integer(class_name),
                id = as.integer(NA),
                metric = "total edge",
                value = as.double(te))
        })

}

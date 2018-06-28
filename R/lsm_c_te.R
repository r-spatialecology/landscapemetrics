#' TE  (class level)
#'
#' @description Total (class) edge (Area and Edge metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary Include landscape boundary in edge length
#'
#' @details
#' \deqn{TE = \sum_{k=1}^{m} e_{ik}}
#' where \eqn{e_{ik}} is the edge lengths in meters

#' TE is an 'Area and edge metric'. Total (class) edge includes all edges between class i and
#' all other classes k. It measures the configuration of the landscape because a highly
#' fragmentated landscape will have many edges. However, total edge is an absolute measure,
#' making comparisons among landscapes with different total areas difficult. If
#' \code{cound_boundary = TRUE} also edges to the landscape boundary are included.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{TE >= 0}
#' \subsection{Behaviour}{Equals TE = 0 if all cells are edge cells. Increases, without limit,
#' as landscape becomes more fragmentated}
#'
#' @seealso
#' \code{\link{lsm_p_perim}}
#' \code{\link{lsm_l_te}}
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

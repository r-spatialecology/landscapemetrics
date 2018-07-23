#'Interspersion and Juxtaposition index (class level)
#
#' @description Interspersion and Juxtaposition index (class level)
#
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param count_boundary ???
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#
#' @details
#' ???
#' \deqn{???}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#
#' @return tibble
#
#' @examples
#'lsm_c_iji(landscape)
#
#' @aliases lsm_c_iji
#' @rdname lsm_c_iji
#
#' @references
#'McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#'Program for Categorical and Continuous Maps. Computer software program produced by
#'the authors at the University of Massachusetts, Amherst. Available at the following
#'web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#
#' @export
lsm_c_iji <- function(landscape,
                      count_boundary, directions) UseMethod("lsm_c_iji")
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterLayer <- function(landscape,
                                  count_boundary = FALSE, directions = 8) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                count_boundary = count_boundary,
                directions = directions,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterStack <- function(landscape,
                                  count_boundary = FALSE,
                                  directions = 8) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                count_boundary = count_boundary,
                directions = directions,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterBrick <- function(landscape,
                                  count_boundary = FALSE,
                                  directions = 8) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                count_boundary = count_boundary,
                directions = directions,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}
#' @name lsm_c_iji
#' @export
lsm_c_iji.list <- function(landscape,
                           count_boundary = FALSE, directions = 8) {
 purrr::map_dfr(raster::as.list(landscape),
                lsm_c_iji_calc,
                count_boundary = count_boundary,
                directions = directions,
                .id = "layer") %>%
     dplyr::mutate(layer = as.integer(layer))
}

# Not working currently
lsm_c_iji_calc <- function(landscape, count_boundary, directions) {


 edges <- rcpp_get_coocurrence_matrix(raster::as.matrix(landscape),
                                      directions = as.matrix(4))
 edges_class <- colSums(edges)

 iji_step1 <- purrr::map(seq_along(edges_class), function(i){

     edges[,i] / edges_class[i]

 })

 iji_step2 <- purrr::map(iji_step1, log)


 iji_step3 <- purrr::map_dbl(seq_along(iji_step1), function(i){

    - sum(iji_step1[[i]] * iji_step2[[i]])
 })

 tibble::tibble(
     level = "class",
     class = as.integer(names(edges_class)),
     id = as.integer(NA),
     metric = "iji",
     value = (iji_step3 / log(length(edges_class - 1))) * 100
 )
}

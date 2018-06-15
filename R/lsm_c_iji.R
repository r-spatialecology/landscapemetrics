#' Interspersion and Juxtaposition index (class level)
#'
#' @description Interspersion and Juxtaposition index (class level)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' ???
#' \deqn{???}
#' \subsection{Units}{???}
#' \subsection{Range}{???}
#' \subsection{Behaviour}{???}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_iji(landscape)
#'
#' @aliases lsm_c_iji
#' @rdname lsm_c_iji
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_iji <- function(landscape, count_boundary) UseMethod("lsm_c_iji")

#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterLayer <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_iji_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterStack <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_iji_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_iji
#' @export
lsm_c_iji.RasterBrick <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_iji_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_iji
#' @export
lsm_c_iji.list <- function(landscape, count_boundary = FALSE) {
    purrr::map_dfr(raster::as.list(landscape),
                   .f = lsm_c_iji_calc,
                   count_boundary = count_boundary,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

# Not working currently
lsm_c_iji_calc <- function(landscape, count_boundary) {

    edge_class <- lsm_c_te(landscape)
    pr <- lsm_l_pr(landscape)

    iji <- landscape %>%
        lsm_p_perim() %>%
        dplyr::mutate(value = (value / edge_class$value[class]) *
                          log((value / edge_class$value[class]))) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = -sum(value)) %>%
        dplyr::mutate(value = (value / log(pr$value - 1)) * 100)

    tibble::tibble(
        level = "class",
        class = iji$class,
        id = as.integer(NA),
        metric = "interspersion and juxtaposition index",
        value = iji$value
    )
}









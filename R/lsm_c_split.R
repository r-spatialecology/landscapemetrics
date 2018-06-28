#' SPLIT (class level)
#'
#' @description Splitting index (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{SPLIT = \frac{A^2}{\sum_{j = 1}^{n} a_{ij}^2}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area.
#'
#' SPLIT is an 'Aggregation metric'. It describes number of patches if all patches of
#' class i would be divided into equally sized patches.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{1 <= SPLIT <= Number of cells squared}
#' \subsection{Behaviour}{Equals SPLIT = 1 if only one patch is present. Increases as
#' the number of patches of class i increases and is limited if all cells are a patch}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_l_split}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_split(landscape)
#'
#' @aliases lsm_c_split
#' @rdname lsm_c_split
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_split <- function(landscape) UseMethod("lsm_c_split")

#' @name lsm_c_split
#' @export
lsm_c_split.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_split
#' @export
lsm_c_split.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_split_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_split_calc <- function(landscape) {

    area_landscape <- lsm_l_ta_calc(landscape)

    split <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (area_landscape$value ^ 2) / value)

    tibble::tibble(
        level = "class",
        class = as.integer(split$class),
        id = as.integer(NA),
        metric = "splitting index",
        value = as.double(split$value)
    )
}

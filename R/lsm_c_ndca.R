#' NDCA (class level)
#'
#' @description Number of disjunct core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{NDCA = \sum \limits_{j = 1}^{n} n_{ij}^{core}}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas.
#'
#' NDCA is a 'Core area metric'. The metric summarises class i as the sum of all
#' patches belonging to class i. A cell is defined as core if the cell has no
#' neighbour with a different value than itself (rook's case). NDCA counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#' It describes patch area and shape simultaneously (more core area when the patch is large,
#' however, the shape must allow disjunct core areas). Thereby, a compact shape (e.g. a square)
#' will contain less disjunct core areas than a more irregular patch.
#'
#' \subsection{Units}{None}
#' \subsection{Range}{NDCA >= 0}
#' \subsection{Behaviour}{NDCA = 0 when TCA = 0, i.e. every cell in patches of class i is
#' an edge. NDCA increases, with out limit, as core area increases and patch shapes allow
#' disjunct core areas (i.e. patch shapes become rather complex).}
#'
#' @seealso
#' \code{\link{lsm_c_tca}}, \cr
#' \code{\link{lsm_p_ncore}},
#' \code{\link{lsm_l_ndca}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_ndca(landscape)
#'
#' @aliases lsm_c_ndca
#' @rdname lsm_c_ndca
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_c_ndca <- function(landscape, directions) UseMethod("lsm_c_ndca")

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_ndca_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_ndca_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_ndca_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_c_ndca_calc,
                   directions = directions,  .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_c_ndca_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ndca_calc <- function(landscape, directions){

    dcad <- landscape %>%
        lsm_p_ncore_calc(directions = directions) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value))

    tibble::tibble(
        level = "class",
        class = as.integer(dcad$class),
        id = as.integer(NA),
        metric = "ndca",
        value = as.double(dcad$value)
    )
}

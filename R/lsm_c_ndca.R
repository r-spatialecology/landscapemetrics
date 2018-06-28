#' NDCA (class level)
#'
#' @description Number of disjunct core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.

#' @details
#' \deqn{NDCA = \sum_{j=1}^{n} n_{ij}^{core}}
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
#' an edge. NDCA increases, with out limit, as core area increases and patch shape allows
#' disjunct core areas (i.e. patch shape becomes rather complex).}
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
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_c_ndca <- function(landscape) UseMethod("lsm_c_ndca")

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_ndca
#' @export
lsm_c_ndca.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_ndca_calc,
                    .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_ndca_calc <- function(landscape){
    dcad <- landscape %>%
        lsm_p_ncore_calc() %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE))

    tibble::tibble(
        level = "class",
        class = as.integer(dcad$class),
        id = as.integer(NA),
        metric = "number of core areas",
        value = as.double(dcad$value)
    )
}

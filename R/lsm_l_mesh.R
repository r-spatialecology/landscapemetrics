#' MESH (landscape level)
#'
#' @description Effective Mesh Size (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which cells should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{MESH = \frac{\sum \limits_{i = 1}^{m} \sum \limits_{j = 1}^{n} a_{ij} ^ 2}{A} * \frac{1} {10000}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area in square meters.
#'
#' The effective mesh size is an 'Aggregation metric'. Because each patch is squared
#' before the sum is calculated and the sum is standardised by the
#' total landscape area, MESH is a relative measure of patch structure. MESH is
#' perfectly, negatively correlated to \code{\link{lsm_c_division}}.
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{cell size / total area <= MESH <= total area}
#' \subsection{Behaviour}{Equals cellsize/total area if class covers only
#' one cell and equals total area if only one patch is present.}
#'
#' @seealso
#' \code{\link{lsm_p_area}},
#' \code{\link{lsm_l_ta}}, \cr
#' \code{\link{lsm_c_mesh}}
#'
#' @return tibble
#'
#' @examples
#' lsm_l_mesh(landscape)
#'
#' @aliases lsm_l_mesh
#' @rdname lsm_l_mesh
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_l_mesh <- function(landscape, directions) UseMethod("lsm_l_mesh")

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mesh_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mesh_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_l_mesh_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_l_mesh
#' @export
lsm_l_mesh.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_l_mesh_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_l_mesh_calc <- function(landscape, directions) {

    area_landscape <- lsm_l_ta_calc(landscape, directions = directions)

    area_patch <- lsm_p_area_calc(landscape, directions = directions)

    mesh <- area_patch %>%
        dplyr::mutate(value = value ^ 2) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (value / area_landscape$value))

    tibble::tibble(
        level = "landscape",
        class = as.integer(NA),
        id = as.integer(NA),
        metric = "mesh",
        value = as.double(mesh$value)
    )
}

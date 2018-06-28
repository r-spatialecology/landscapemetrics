#' MESH (class level)
#'
#' @description Effective Mesh Size (Aggregation metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{MESH = \frac{\sum_{j = 1}^{n} a_{ij} ^ 2}{A} * \frac{1}{10000}}
#' where \eqn{a_{ij}} is the patch area in square meters and \eqn{A} is the
#' total landscape area in square meters.
#'
#' The effective mesh size is an 'Aggregation metric'. Because each patch is squared
#' before the sums for each group i are calculated and the sum is standardised by the
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
#' \code{\link{lsm_l_mesh}}
#'
#' @return tibble
#'
#' @examples
#' lsm_c_mesh(landscape)
#'
#' @aliases lsm_c_mesh
#' @rdname lsm_c_mesh
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_c_mesh <- function(landscape) UseMethod("lsm_c_mesh")

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_c_mesh
#' @export
lsm_c_mesh.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_c_mesh_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_c_mesh_calc <- function(landscape) {

    total_area <- landscape %>%
        lsm_l_ta_calc() %>%
        dplyr::mutate(value = value * 10000)

    mesh <- landscape %>%
        lsm_p_area_calc() %>%
        dplyr::mutate(value = (value * 10000) ^ 2) %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
        dplyr::mutate(value = (value / total_area$value) * (1 / 10000))

    tibble::tibble(
        level = "class",
        class = as.integer(mesh$class),
        id = as.integer(NA),
        metric = "effective mesh size",
        value = as.double(mesh$value)
    )
}

#' CORE (patch level)
#'
#' @description Core area (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{CORE = a_{ij}^{core}}
#' where \eqn{a_{ij}^{core}} is the core area in square meters
#'
#' CORE is a 'Core area metric' and equals the area within a patch that is not
#' on the edge of it. A cell is defined as core area if the cell has no
#' neighbour with a different value than itself (rook's case). It describes patch area
#' and shape simultaneously (more core area when the patch is large and the shape is
#' rather compact, i.e. a square).
#'
#' \subsection{Units}{Hectares}
#' \subsection{Range}{CORE >= 0}
#' \subsection{Behaviour}{Increases, without limit, as the patch area increases
#' and the patch shape simplifies (more core area). CORE = 0 when every cell in
#' the patch is an edge.}
#'
#' @seealso
#' \code{\link{lsm_c_core_mn}},
#' \code{\link{lsm_c_core_sd}},
#' \code{\link{lsm_c_core_cv}},
#' \code{\link{lsm_c_tca}}, \cr
#' \code{\link{lsm_l_core_mn}},
#' \code{\link{lsm_l_core_sd}},
#' \code{\link{lsm_l_core_cv}},
#' \code{\link{lsm_l_tca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#'
#' @examples
#' lsm_p_core(landscape)
#'
#' @aliases lsm_p_core
#' @rdname lsm_p_core
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_core <- function(landscape, directions) UseMethod("lsm_p_core")

#' @name lsm_p_core
#' @export
lsm_p_core.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_core_calc, directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_core_calc, directions = directions,.id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_core_calc, directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_core
#' @export
lsm_p_core.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_p_core_calc, directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_p_core_calc <- function(landscape, directions) {
    landscape_labelled <- cclabel(landscape, directions = directions)

    core <-
        purrr::map_dfr(landscape_labelled, function(patches_class) {

            cells_patch <- patches_class %>%
                raster::values() %>%
                table()

            boundary <- raster::boundaries(patches_class,
                                           directions = 4)

            boundary_patch <- table(raster::values(patches_class)[raster::values(boundary) == 1])

            core_area <-
                (cells_patch - boundary_patch) *
                prod(raster::res(patches_class)) / 10000

            class_name <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            tibble::tibble(class = class_name,
                           value = core_area)
        })

    tibble::tibble(
        level = "patch",
        class = as.integer(core$class),
        id = as.integer(seq_len(nrow(core))),
        metric = "core",
        value = as.double(core$value)
    )
}


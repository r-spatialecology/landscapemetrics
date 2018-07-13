#' NCORE (patch level)
#'
#' @description Number of core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' #' @details
#' \deqn{NCORE = n_{ij}^{core}}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas.
#'
#' NCORE is a 'Core area metric'. A cell is defined as core if the cell has no
#' neighbour with a different value than itself (rook's case). The metric counts the disjunct
#' core areas, whereby a core area is a 'patch within the patch' containing only core cells.
#' It describes patch area and shape simultaneously (more core area when the patch is large,
#' however, the shape must allow disjunct core areas). Thereby, a compact shape (e.g. a square)
#' will contain less disjunct core areas than a more irregular patch.
#
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patch is edge.
#' Increases, without limit, as core area increases and patch shape allows disjunct core
#' areas (i.e. patch shape becomes rather complex).}
#'
#' @seealso
#' \code{\link{lsm_c_dcore_mn}},
#' \code{\link{lsm_c_dcore_sd}},
#' \code{\link{lsm_c_dcore_cv}},
#' \code{\link{lsm_c_ndca}}, \cr
#' \code{\link{lsm_l_dcore_mn}},
#' \code{\link{lsm_l_dcore_sd}},
#' \code{\link{lsm_l_dcore_cv}},
#' \code{\link{lsm_l_ndca}}
#'
#' @return tibble
#'
#' @importFrom stats na.omit
#' @importFrom raster ncell
#'
#' @examples
#' lsm_p_ncore(landscape)
#'
#' @aliases lsm_p_ncore
#' @rdname lsm_p_ncore
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_ncore <- function(landscape) UseMethod("lsm_p_ncore")

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_ncore_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_ncore_calc <- function(landscape){

    landscape_labelled <- cclabel(landscape)

    landscape_extent <- raster::extent(landscape)
    landscape_raster <- raster::raster(landscape_extent,
                        resolution = raster::res(landscape),
                        crs = raster::crs(landscape))

    core_class <- purrr::map_dfr(landscape_labelled, function(patches_class) {

        patches_landscape <- patches_class %>%
            raster::values() %>%
            stats::na.omit() %>%
            unique()

        class_name <- patches_class %>%
            names() %>%
            sub("Class_", "", .)

        class_boundary <- raster::boundaries(patches_class, directions = 4)

        raster::values(class_boundary)[raster::values(class_boundary) == 1 |
                            raster::values(is.na(class_boundary))] <- -999

        cclabel_landscape <- cclabel(class_boundary)[[2]]

        points <- raster::rasterToPoints(cclabel_landscape)
        points <- matrix(points[!duplicated(points[, 3]),], ncol = 3)

        n_core_area <- table(raster::extract(x = patches_class,
                                             y = points[, 1:2]))

        result <- c(rep(0, length(patches_landscape)))
        names(result)  <- patches_landscape

        result[as.numeric(names(n_core_area))] <- n_core_area

        (tibble::tibble(
            class = class_name,
            value = result
        ))
    })

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = as.integer(seq_len(nrow(core_class))),
        metric = "ncore",
        value = as.double(core_class$value)
    )
}

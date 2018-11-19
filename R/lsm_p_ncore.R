#' NCORE (patch level)
#'
#' @description Number of core areas (Core area metric)
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' #' @details
#' \deqn{NCORE = n_{ij}^{core}}
#' where \eqn{n_{ij}^{core}} is the number of disjunct core areas.
#'
#' NCORE is a 'Core area metric'. A cell is defined as core if the cell has no
#' neighbour with a different value than itself (rook's case). The metric
#' counts the disjunct core areas, whereby a core area is a 'patch within the
#' patch' containing only core cells. It describes patch area and shape
#' simultaneously (more core area when the patch is large, however, the shape
#' must allow disjunct core areas). Thereby, a compact shape (e.g. a square)
#' will contain less disjunct core areas than a more irregular patch.
#
#' \subsection{Units}{None}
#' \subsection{Range}{NCORE >= 0}
#' \subsection{Behaviour}{NCORE = 0 when CORE = 0, i.e. every cell in patch is
#' edge. Increases, without limit, as core area increases and patch shape
#' allows disjunct core areas (i.e. patch shape becomes rather complex).}
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
lsm_p_ncore <- function(landscape, directions, consider_boundary, edge_depth) UseMethod("lsm_p_ncore")

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterLayer <- function(landscape,
                                    directions = 8,
                                    consider_boundary = FALSE,
                                    edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterStack <- function(landscape,
                                    directions = 8,
                                    consider_boundary = FALSE,
                                    edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterBrick <- function(landscape,
                                    directions = 8,
                                    consider_boundary = FALSE,
                                    edge_depth = 1) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.stars <- function(landscape,
                              directions = 8,
                              consider_boundary = FALSE,
                              edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.list <- function(landscape,
                             directions = 8,
                             consider_boundary = FALSE,
                             edge_depth = 1) {

    result <- lapply(X = landscape,
                     FUN = lsm_p_ncore_calc,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth)

    dplyr::mutate(dplyr::bind_rows(result, .id = "layer"),
                  layer = as.integer(layer))
}

lsm_p_ncore_calc <- function(landscape, directions, consider_boundary, edge_depth){

    landscape_labeled <- get_patches(landscape, directions = directions)

    landscape_extent <- raster::extent(landscape)
    landscape_raster <- raster::raster(landscape_extent,
                                       resolution = raster::res(landscape),
                                       crs = raster::crs(landscape))

    core_class <- lapply(landscape_labeled, function(patches_class) {

        class <- sub("Class_", "", names(patches_class))

        if(!isTRUE(consider_boundary)) {
            patches_class <- pad_raster(patches_class, pad_raster_value = NA,
                                        pad_raster_cells = 1,
                                        global = FALSE)
        }

        patches_id <- unique(stats::na.omit(raster::values(patches_class)))

        class_edge <- raster::boundaries(patches_class, directions = 4)

        if(edge_depth > 1){
            for(i in seq_len(edge_depth - 1)){

                raster::values(class_edge)[raster::values(class_edge) == 1] <- NA

                class_edge <- raster::boundaries(class_edge,
                                                 directions = 4)
            }
        }

        raster::values(class_edge)[raster::values(class_edge) == 1 | raster::values(is.na(class_edge))] <- -999

        n_boundary <- length(unique(raster::values(class_edge)))

        if(n_boundary == 1){
            result <- c(rep(0, length(patches_id)))
            names(result)  <- patches_id
        }

        else{
            patch_core <- get_patches(class_edge,
                                      directions = directions, class = 0)[[1]]

            points <- raster::rasterToPoints(patch_core)
            points <- matrix(points[!duplicated(points[, 3]),], ncol = 3)

            n_core_area <- table(raster::extract(x = patches_class,
                                                 y = matrix(points[, 1:2],
                                                            ncol = 2)))

            result <- c(rep(0, length(patches_id)))
            names(result)  <- patches_id

            result[as.numeric(names(n_core_area))] <- n_core_area
        }

        tibble::tibble(
            class = class,
            value = result
        )
    })

    core_class <- dplyr::bind_rows(core_class)

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = as.integer(seq_len(nrow(core_class))),
        metric = "ncore",
        value = as.double(core_class$value)
    )
}

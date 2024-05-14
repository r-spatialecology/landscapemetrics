#' PERIM (patch level)
#'
#' @description Perimeter (Area and edge metric)
#'
#' @param landscape A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PERIM = p_{ij}}
#' where \eqn{p_{ij}} is the perimeter in meters.
#'
#' PERIM is an 'Area and edge metric'. It equals the perimeter of the patch
#' including also the edge to the landscape boundary. The metric describes
#' patch area (larger perimeter for larger patches), but also patch shape
#' (large perimeter for irregular shapes). Because the metric is based on
#' distances or areas please make sure your data is valid using \code{\link{check_landscape}}.
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{PERIM > 0}
#' \subsection{Behaviour}{Increases, without limit, as patch size and
#' complexity increases.}
#'
#' @return tibble
#'
#' @examples
#' landscape <- terra::rast(landscapemetrics::landscape)
#' lsm_p_perim(landscape)
#'
#' @references
#' McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical Maps. Computer software program produced by the authors;
#' available at the following web site: https://www.fragstats.org
#'
#' @export
lsm_p_perim <- function(landscape, directions = 8) {
    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = lsm_p_perim_calc,
                     directions = directions)

    layer <- rep(seq_along(result),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    tibble::add_column(result, layer, .before = TRUE)
}

lsm_p_perim_calc <- function(landscape, directions, resolution, extras = NULL) {

    if (missing(resolution)) resolution <- terra::res(landscape)

    if (is.null(extras)){
        metrics <- "lsm_p_perim"
        landscape <- terra::as.matrix(landscape, wide = TRUE)
        extras <- prepare_extras(metrics, landscape_mat = landscape,
                                            directions = directions, resolution = resolution)
    }

    # all values NA
    if (all(is.na(landscape))) {
        return(tibble::new_tibble(list(level = "patch",
                              class = as.integer(NA),
                              id = as.integer(NA),
                              metric = "perim",
                              value = as.double(NA))))
    }

    perimeter_patch <- extras$perimeter_patch

    tibble::new_tibble(list(
        level = rep("patch", nrow(perimeter_patch)),
        class = as.integer(perimeter_patch$class),
        id = as.integer(seq_len(nrow(perimeter_patch))),
        metric = rep("perim", nrow(perimeter_patch)),
        value = as.double(perimeter_patch$value)
    ))
}

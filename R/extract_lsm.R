#' extract_lsm
#'
#' @description Extract metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param y Spatial object ( Spatialy*; SpatialPolygons*; SpatialLines; Extent or sf equivalents); two-column matrix/data.frame/tibble or cellnumbers that are used to extract landscapemetrics.
#' @param what String indicating what metric to calculate, either "patch" (default) for all patch level metrics or any of the patch metrics functions.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#' @param full_name Should the full names of all functions be included in the
#' tibble.
#' @param verbose Print warning message if not sufficient patches are present
#' @param progress Print progress report
#' @param ... Arguments passed to \code{raster::extract}.
#'
#' @details
#' This functions extracts the metrics of all patches the spatial object(s) `y`
#' (e.g. spatial points) are located within. Only patch level metrics are possible
#' to extract. Please be aware that the output is sligthly different to all
#' other `lsm`-function of `landscapemetrics`. Returns a tibble with chosen
#' metrics and the ID of the spatial objects.
#'
#' @seealso
#' \code{\link{lsm_c_enn_mn}},
#' \code{\link{lsm_c_enn_sd}},
#' \code{\link{lsm_c_enn_cv}}, \cr
#' \code{\link{lsm_l_enn_mn}},
#' \code{\link{lsm_l_enn_sd}},
#' \code{\link{lsm_l_enn_cv}}
#'
#' @return tibble
#'
#' @examples
#' points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' extract_lsm(landscape, points, what = "lsm_p_area")
#'
#' points_sp <- sp::SpatialPoints(points)
#' extract_lsm(landscape, points, what = "lsm_p_area")
#'
#' @aliases extract_lsm
#' @rdname extract_lsm
#'
#' @export
extract_lsm <- function(landscape,
                        y,
                        directions = 8,
                        consider_boundary,
                        edge_depth,
                        full_name,
                        verbose,
                        progress,
                        ...) UseMethod("extract_lsm")

#' @name extract_lsm
#' @export
extract_lsm.RasterLayer <- function(landscape,
                                    y,
                                    what = "patch",
                                    directions = 8,
                                    consider_boundary = FALSE,
                                    edge_depth = 1,
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

    result <- extract_lsm_int(landscape,
                    y = y,
                    what = what,
                    directions = directions,
                    consider_boundary = consider_boundary,
                    edge_depth = edge_depth,
                    full_name = full_name,
                    verbose = verbose,
                    progress = progress,
                    ...)

    # result <- result[, c(1, 7, 2, 3, 4, 5, 6)]
    return(result)
}

#' @name extract_lsm
#' @export
extract_lsm.RasterStack <- function(landscape,
                                    y,
                                    what = "patch",
                                    directions = 8,
                                    edge_depth = 1,
                                    consider_boundary = FALSE,
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

  result <- lapply(X = raster::as.list(landscape),
         FUN = extract_lsm_int,
         y = y,
         what = what,
         directions = directions,
         consider_boundary = consider_boundary,
         edge_depth = edge_depth,
         full_name = full_name,
         verbose = verbose,
         progress = progress,
         ...)

  for(current_layer in seq_along(result)) {
      result[[current_layer]]$layer <- current_layer
  }

  result <- dplyr::bind_rows(result)

  # result <- result[, c(1, 7, 2, 3, 4, 5, 6)]
  return(result)
}

#' @name extract_lsm
#' @export
extract_lsm.RasterBrick <- function(landscape,
                                    y,
                                    what = "patch",
                                    directions = 8,
                                    edge_depth = 1,
                                    consider_boundary = FALSE,
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     what = what,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    for(current_layer in seq_along(result)) {
        result[[current_layer]]$layer <- current_layer
    }

    result <- dplyr::bind_rows(result)

    # result <- result[, c(1, 7, 2, 3, 4, 5, 6)]
    return(result)
}

#' @name extract_lsm
#' @export
extract_lsm.stars <- function(landscape,
                              y,
                              what = "patch",
                              directions = 8,
                              edge_depth = 1,
                              consider_boundary = FALSE,
                              full_name = FALSE,
                              verbose = TRUE,
                              progress = FALSE,
                              ...) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     what = what,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    for(current_layer in seq_along(result)) {
        result[[current_layer]]$layer <- current_layer
    }

    result <- dplyr::bind_rows(result)

    # result <- result[, c(1, 7, 2, 3, 4, 5, 6)]
    return(result)
}

#' @name extract_lsm
#' @export
extract_lsm.list <- function(landscape,
                             y,
                             what = "patch",
                             directions = 8,
                             edge_depth = 1,
                             consider_boundary = FALSE,
                             full_name = FALSE,
                             verbose = TRUE,
                             progress = FALSE,
                             ...) {

    result <- lapply(X = landscape,
                     FUN = extract_lsm_int,
                     y = y,
                     what = what,
                     directions = directions,
                     consider_boundary = consider_boundary,
                     edge_depth = edge_depth,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    for(current_layer in seq_along(result)) {
        result[[current_layer]]$layer <- current_layer
    }

    result <- dplyr::bind_rows(result)

    # result <- result[, c(1, 7, 2, 3, 4, 5, 6)]
    return(result)
}

extract_lsm_int <- function(landscape,
                            y,
                            what,
                            directions,
                            edge_depth,
                            consider_boundary,
                            full_name,
                            verbose,
                            progress,
                            ...) {

  namespace_patch <- getNamespaceExports("landscapemetrics")
  namespace_patch <- namespace_patch[namespace_patch %in%
                                       grep("_p_", namespace_patch,
                                            value = TRUE)]
  namespace_patch <- namespace_patch[!grepl("\\.|calc", namespace_patch)]
  namespace_patch <- namespace_patch[!grepl("\\.|int", namespace_patch)]
  namespace_patch <- append(namespace_patch, "patch")
  if (!any(what %in% namespace_patch)){
    stop("extract_lsm only takes patch level metrics as what argument.")
  }

  if (any(class(y) %in% c("MULTIPOINT",
                          "POINT"))) {
    y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)
  } else if (any(class(y) %in% c("sf",
                                 "sfc"))) {
    if (all(sf::st_geometry_type(y) %in% c("POINT", "MULTIPOINT"))) {
      y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)
    } else {
      stop(
        "landscapemetrics currently only supports point features for landscape metrics extraction."
      )
    }

  } else if (any(class(y) %in% c("LINESTRING",
                                 "POLYGON",
                                 "MULTILINESTRING",
                                 "MULTIPOLYGON"))) {
    stop(
      "landscapemetrics currently only supports point features for landscape metrics extraction."
    )
  }

    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)) {

        max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }

    landscape_id <- raster::merge(raster::stack(landscape_labeled))

    point_id <- raster::extract(landscape_id, y, df = TRUE, ...)
    names(point_id) <- c("extract_id", "id")

    metrics <- calculate_lsm(landscape,
                             what = what,
                             directions = directions,
                             edge_depth = edge_depth,
                             consider_boundary = consider_boundary,
                             full_name = full_name,
                             verbose = verbose,
                             progress = progress)

    extract_metrics <- dplyr::mutate(dplyr::left_join(metrics, point_id, by = "id"),
                                     extract_id = as.integer(extract_id))

    extract_metrics <- dplyr::filter(extract_metrics, !is.na(extract_id))

    extract_metrics <- dplyr::arrange(extract_metrics, layer, extract_id)

    extract_metrics[, c(1, 7, 2, 3, 4, 5, 6)]

}

#' extract_lsm
#'
#' @description Extract landscapemetrics for certain cells
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param y Spatial object ( Spatialy*; SpatialPolygons*; SpatialLines; Extent or sf equivalents); two-column matrix/data.frame/tibble or cellnumbers that are used to extract landscapemetrics.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param count_boundary Include landscape boundary in edge length
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core
#' @param classes_max Potential maximum number of present classes
#' @param neighbourhood The number of directions in which cell adjacencies are considered as neighbours:
#' 4 (rook's case) or 8 (queen's case). The default is 4.
#' @param ordered The type of pairs considered. Either ordered (TRUE) or unordered (FALSE).
#' The default is TRUE.
#' @param base The unit in which entropy is measured. The default is "log2",
#' which compute entropy in "bits". "log" and "log10" can be also used.
#' @param full_name Should the full names of all functions be included in the
#' tibble.
#' @param verbose Print warning message if not sufficient patches are present
#' @param progress Print progress report
#' @param ... Arguments passed to \code{raster::extract}.
#'
#' @details
#' Extracts landscapemetrics for spatial objects. Returns a tibble with
#' landscapemetrics on patch level and the ID for the spatial object that was
#' used for extracting.
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
#' points <- raster::sampleRandom(landscape, 20, sp = TRUE)
#' extract_lsm(landscape, points)
#'
#' @aliases extract_lsm
#' @rdname extract_lsm
#'
#' @export
extract_lsm <- function(landscape,
                        y,
                        directions = 8,
                        count_boundary,
                        consider_boundary,
                        classes_max,
                        neighbourhood,
                        ordered,
                        base,
                        full_name,
                        verbose,
                        progress,
                        ...) UseMethod("extract_lsm")

#' @name extract_lsm
#' @export
extract_lsm.RasterLayer <- function(landscape,
                                    y,
                                    directions = 8,
                                    count_boundary = FALSE,
                                    consider_boundary = FALSE,
                                    classes_max = NULL,
                                    neighbourhood = 4,
                                    ordered = TRUE,
                                    base = "log2",
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

    lapply(X = raster::as.list(landscape),
           FUN = extract_lsm_int,
           y = y,
           directions = directions,
           count_boundary = count_boundary,
           consider_boundary = consider_boundary,
           classes_max = classes_max,
           neighbourhood = neighbourhood,
           ordered = ordered,
           base = base,
           full_name = full_name,
           verbose = verbose,
           progress = progress,
           ...)[[1]]

}

#' @name extract_lsm
#' @export
extract_lsm.RasterStack <- function(landscape,
                                    y,
                                    directions = 8,
                                    count_boundary = FALSE,
                                    consider_boundary = FALSE,
                                    classes_max = NULL,
                                    neighbourhood = 4,
                                    ordered = TRUE,
                                    base = "log2",
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

  result <- lapply(X = raster::as.list(landscape),
         FUN = extract_lsm_int,
         y = y,
         directions = directions,
         count_boundary = count_boundary,
         consider_boundary = consider_boundary,
         classes_max = classes_max,
         neighbourhood = neighbourhood,
         ordered = ordered,
         base = base,
         full_name = full_name,
         verbose = verbose,
         progress = progress,
         ...)

  dplyr::mutate(dplyr::bind_rows(result),
                layer = as.integer(layer))
}

#' @name extract_lsm
#' @export
extract_lsm.RasterBrick <- function(landscape,
                                    y,
                                    directions = 8,
                                    count_boundary = FALSE,
                                    consider_boundary = FALSE,
                                    classes_max = NULL,
                                    neighbourhood = 4,
                                    ordered = TRUE,
                                    base = "log2",
                                    full_name = FALSE,
                                    verbose = TRUE,
                                    progress = FALSE,
                                    ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    dplyr::mutate(dplyr::bind_rows(result),
                  layer = as.integer(layer))
}

#' @name extract_lsm
#' @export
extract_lsm.stars <- function(landscape,
                              y,
                              directions = 8,
                              count_boundary = FALSE,
                              consider_boundary = FALSE,
                              classes_max = NULL,
                              neighbourhood = 4,
                              ordered = TRUE,
                              base = "log2",
                              full_name = FALSE,
                              verbose = TRUE,
                              progress = FALSE,
                              ...) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    dplyr::mutate(dplyr::bind_rows(result),
                  layer = as.integer(layer))
}

#' @name extract_lsm
#' @export
extract_lsm.list <- function(landscape,
                             y,
                             directions = 8,
                             count_boundary = FALSE,
                             consider_boundary = FALSE,
                             classes_max = NULL,
                             neighbourhood = 4,
                             ordered = TRUE,
                             base = "log2",
                             full_name = FALSE,
                             verbose = TRUE,
                             progress = FALSE,
                             ...) {

    result <- lapply(X = landscape,
                     FUN = extract_lsm_int,
                     y = y,
                     directions = directions,
                     count_boundary = count_boundary,
                     consider_boundary = consider_boundary,
                     classes_max = classes_max,
                     neighbourhood = neighbourhood,
                     ordered = ordered,
                     base = base,
                     full_name = full_name,
                     verbose = verbose,
                     progress = progress,
                     ...)

    dplyr::mutate(dplyr::bind_rows(result),
                  layer = as.integer(layer))
}

extract_lsm_int <- function(landscape,
                            y,
                            directions,
                            count_boundary,
                            consider_boundary,
                            classes_max,
                            neighbourhood,
                            ordered,
                            base,
                            full_name,
                            verbose,
                            progress,
                            ...) {

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
                                 what = "patch",
                                 directions = directions,
                                 count_boundary = count_boundary,
                                 consider_boundary = consider_boundary,
                                 classes_max = classes_max,
                                 neighbourhood = neighbourhood,
                                 ordered = ordered,
                                 base = base,
                                 full_name = full_name,
                                 verbose = verbose,
                                 progress = progress)

    extract_metrics <- dplyr::left_join(metrics, point_id, by = "id")
    dplyr::filter(extract_metrics, !is.na(extract_id))

}

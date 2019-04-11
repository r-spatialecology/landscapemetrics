#' extract_lsm
#'
#' @description Extract metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param y 2-column matrix with coordinates, SpatialPoints, SpatialLines or sf point geometries.
#' @param metric Abbreviation of metrics to calculate (e.g. 'area').
#' @param name Full name of metrics to calculate (e.g. 'core area').
#' @param type Metric types to calculate according to FRAGSTATS grouping (e.g. 'aggregation metric').
#' @param what String indicating what metric to calculate, either "patch" (default) for all patch level metrics or any of the patch metrics functions.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param ... Arguments passed to \code{calculate_lsm()}.
#'
#' @details
#' This functions extracts the metrics of all patches the spatial object(s) `y`
#' (e.g. spatial points) are located within. Only patch level metrics are possible
#' to extract. Please be aware that the output is sligthly different to all
#' other `lsm`-function of `landscapemetrics`. Returns a tibble with chosen
#' metrics and the ID of the spatial objects.
#'
#' @seealso
#' \code{\link{calculate_lsm}}
#'
#' @return tibble
#'
#' @examples
#' points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
#' extract_lsm(landscape, y = points)
#' extract_lsm(landscape, y = points, type = "aggregation metric")
#'
#' points_sp <- sp::SpatialPoints(points)
#' extract_lsm(landscape, y = points_sp, what = "lsm_p_area")
#'
#' \dontrun{
#' # use lines (works only if rgeos is installed)
#' x1 <- c(1, 5, 15, 10)
#' y1 <- c(1, 5, 15, 25)
#'
#' x2 <- c(10, 25)
#' y2 <- c(5, 5)
#'
#' sample_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(x1, y1)),
#' sp::Line(cbind(x2, y2))), ID = "a")))
#' extract_lsm(landscape, y = sample_lines, what = "lsm_p_area")
#' }
#'
#' @aliases extract_lsm
#' @rdname extract_lsm
#'
#' @export
extract_lsm <- function(landscape,
                        y,
                        metric, name, type, what,
                        directions,
                        ...) UseMethod("extract_lsm")

#' @name extract_lsm
#' @export
extract_lsm.RasterLayer <- function(landscape,
                                    y,
                                    metric = NULL,
                                    name = NULL,
                                    type = NULL,
                                    what = NULL,
                                    directions = 8,
                                    ...) {

  result <- lapply(raster::as.list(landscape),
                   FUN = extract_lsm_int,
                   y = y,
                   metric = metric,
                   name = name,
                   type = type,
                   what = what,
                   directions = directions,
                   ...)

  layer <- rep(seq_len(length(result)),
               vapply(result, nrow, FUN.VALUE = integer(1)))

  result <- do.call(rbind, result)

  result$layer <- layer

  result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

#' @name extract_lsm
#' @export
extract_lsm.RasterStack <- function(landscape,
                                    y,
                                    metric = NULL,
                                    name = NULL,
                                    type = NULL,
                                    what = NULL,
                                    directions = 8,
                                    ...) {

  result <- lapply(X = raster::as.list(landscape),
                   FUN = extract_lsm_int,
                   y = y,
                   metric = metric,
                   name = name,
                   type = type,
                   what = what,
                   directions = directions,
                   ...)

  layer <- rep(seq_len(length(result)),
               vapply(result, nrow, FUN.VALUE = integer(1)))

  result <- do.call(rbind, result)

  result$layer <- layer

  result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

#' @name extract_lsm
#' @export
extract_lsm.RasterBrick <- function(landscape,
                                    y,
                                    metric = NULL,
                                    name = NULL,
                                    type = NULL,
                                    what = NULL,
                                    directions = 8,
                                    ...) {

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     directions = directions,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

#' @name extract_lsm
#' @export
extract_lsm.stars <- function(landscape,
                              y,
                              metric = NULL,
                              name = NULL,
                              type = NULL,
                              what = NULL,
                              directions = 8,
                              ...) {

    landscape <- methods::as(landscape, "Raster")

    result <- lapply(X = raster::as.list(landscape),
                     FUN = extract_lsm_int,
                     y = y,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     directions = directions,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

#' @name extract_lsm
#' @export
extract_lsm.list <- function(landscape,
                             y,
                             metric = NULL,
                             name = NULL,
                             type = NULL,
                             what = NULL,
                             directions = 8,
                             ...) {

    result <- lapply(X = landscape,
                     FUN = extract_lsm_int,
                     y = y,
                     metric = metric,
                     name = name,
                     type = type,
                     what = what,
                     directions = directions,
                     ...)

    layer <- rep(seq_len(length(result)),
                 vapply(result, nrow, FUN.VALUE = integer(1)))

    result <- do.call(rbind, result)

    result$layer <- layer

    result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

extract_lsm_int <- function(landscape,
                            y,
                            metric, name, type, what,
                            directions,
                            ...) {
  # get all patch level metrics if not specified
  if (is.null(what)) {
    level <- "patch"
  }

  # what already specified
  else {
    level <- NULL
  }

  # get list of metrics to calculate
  metrics_list <- landscapemetrics::list_lsm(level = level,
                                             metric = metric,
                                             name = name,
                                             type = type,
                                             what = what,
                                             simplify = TRUE,
                                             verbose = FALSE)

  # check if only patch level metrics are selected
  if (!all(metrics_list %in% list_lsm(level = "patch", simplify = TRUE))) {

    stop("'extract_lsm()' only takes patch level metrics.", call. = FALSE)
  }

  # check if sf object is provided
  if (methods::is(y, "sf")) {

    # check if points have the right class
    if (any(class(y) %in% c("MULTIPOINT", "POINT"))) {

      y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)
    }

    else if (any(class(y) %in% c("sf", "sfc"))) {

      if (all(sf::st_geometry_type(y) %in% c("POINT", "MULTIPOINT"))) {

        y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)
      }

      else {

        stop(
          "landscapemetrics currently only supports point features for landscape metrics extraction."
        )
      }
    }

    else if (any(class(y) %in% c("LINESTRING", "POLYGON", "MULTILINESTRING", "MULTIPOLYGON"))) {

      stop(
        "landscapemetrics currently only supports point features for landscape metrics extraction."
      )
    }
  }

  # if Spatial Lines disaggregate
  else if (methods::is(y, "SpatialLines")) {

    y <- sp::disaggregate(y)
  }

  else if (!methods::is(y, "matrix") & !methods::is(y, "SpatialPoints") & !methods::is(y, "SpatialPointsDataFrame")) {

    stop("'y' must be a matrix, SpatialPoints, SpatialLines or sf point geometries.",
         call. = FALSE)
  }

  # get patches of landscape
  landscape_labeled <- get_patches(landscape,
                                   directions = directions,
                                   class = "all",
                                   to_disk = getOption("to_disk", default = FALSE),
                                   return_raster = TRUE)

  # label patch id continuously
  for (i in seq_len(length(landscape_labeled) - 1)) {

    # max patch id of current layer
    max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

    # add max patch id to ids of next layer
    landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
  }

  # combine to one raster layer
  landscape_id <- raster::merge(raster::stack(landscape_labeled))

  # get patch id of sample points
  point_id <- raster::extract(x = landscape_id,
                              y = y,
                              df = TRUE)

  # rename df
  names(point_id) <- c("extract_id", "id")

  point_id <- point_id[!duplicated(point_id), ]

  # calculate metrics
  # can we somehow calculate only the patches we actually want?
  metrics <- calculate_lsm(landscape,
                           what = metrics_list,
                           ...)

  # only patchs that contain a sample point
  extract_metrics <- merge(x = metrics, y = point_id,
                           by = "id",
                           all.x = FALSE, all.y = FALSE, sort = FALSE)

  # order cols
  extract_metrics <- extract_metrics[, c(names(metrics), "extract_id")]

  # order rows
  tibble::as_tibble(extract_metrics)
}

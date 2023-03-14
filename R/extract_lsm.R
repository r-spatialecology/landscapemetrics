#' extract_lsm
#'
#' @description Extract metrics
#'
#' @param landscape Raster* Layer, Stack, Brick, SpatRaster (terra), stars, or a list of rasterLayers.
#' @param y 2-column matrix with coordinates or sf point or line geometries.
#' @param extract_id Vector with id of sample points. If not provided, sample
#' points will be labelled 1...n.
#' @param metric Abbreviation of metrics (e.g. 'area').
#' @param name Full name of metrics (e.g. 'core area')
#' @param type Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').
#' @param what Selected level of metrics: either "patch", "class" or "landscape".
#' It is also possible to specify functions as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param progress Print progress report.
#' @param verbose Print warning messages.
#' @param ... Arguments passed to \code{calculate_lsm()}.
#'
#' @details
#' This functions extracts the metrics of all patches the spatial object(s) `y`
#' (e.g. spatial points) are located within. Only patch level metrics are possible
#' to extract. Please be aware that the output is slightly different to all
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
#' extract_lsm(terra::unwrap(landscape), y = points)
#' extract_lsm(terra::unwrap(landscape), y = points, type = "aggregation metric")
#'
#' \dontrun{
#' # use lines
#'
#' }
#'
#' @aliases extract_lsm
#' @rdname extract_lsm
#'
#' @export
extract_lsm <- function(landscape, y,
                        extract_id = NULL, metric = NULL,
                        name = NULL, type = NULL, what = NULL, directions = 8,
                        progress = FALSE, verbose = TRUE, ...) {

  landscape <- landscape_as_list(landscape)

  result <- lapply(X = seq_along(landscape), FUN = function(x) {

    if (progress) {

      cat("\r> Progress nlayers: ", x , "/", length(landscape))
    }

    extract_lsm_internal(landscape = landscape[[x]],
                         y = y,
                         extract_id = extract_id,
                         metric = metric,
                         name = name,
                         type = type,
                         what = what,
                         directions = directions,
                         progress = FALSE,
                         verbose = verbose,
                         ...)
  })

  layer <- rep(seq_along(result),
               vapply(result, nrow, FUN.VALUE = integer(1)))

  result <- do.call(rbind, result)

  result$layer <- layer

  if (progress) {cat("\n")}

  result[with(result, order(layer, extract_id, level, metric, class, id)), ]
}

extract_lsm_internal <- function(landscape,
                                 y,
                                 extract_id,
                                 metric, name, type, what,
                                 directions,
                                 progress,
                                 verbose,
                                 ...) {

  # get list of metrics to calculate
  metrics_list <- list_lsm(level = "patch",
                           metric = metric,
                           name = name,
                           type = type,
                           what = what,
                           simplify = TRUE,
                           verbose = verbose)

  # check if only patch level metrics are selected
  if (!all(metrics_list %in% list_lsm(level = "patch", simplify = TRUE))) {

    stop("'extract_lsm()' only takes patch level metrics.", call. = FALSE)
  }

  # check if sf object is provided
  if (inherits(x = y, what = "sf")) {

    # check if points have the right class
    if (inherits(x = y, what = c("MULTIPOINT", "POINT"))) {

      y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)

    }

    else if (inherits(x = y, what = c("sf", "sfc"))) {

      if (all(sf::st_geometry_type(y) %in% c("POINT", "MULTIPOINT"))) {

        y <- matrix(sf::st_coordinates(y)[, 1:2], ncol = 2)
      }

      else {

        stop(
          "landscapemetrics currently only supports point features for landscape metrics extraction."
        )
      }
    }

    else if (inherits(x = y, what = c("LINESTRING", "POLYGON",
                                      "MULTILINESTRING", "MULTIPOLYGON"))) {

      stop(
        "landscapemetrics currently only supports sf point features for landscape metrics extraction."
      )
    }
  }

  else if (!inherits(x = y, what = "matrix")) {

    stop("'y' must be a matrix or sf object.",
         call. = FALSE)
  }

  # get patches of landscape
  landscape_labeled <- get_patches(landscape, directions = directions,)[[1]]

  # combine to one raster layer
  landscape_id <- sum(terra::rast(landscape_labeled), na.rm = TRUE)

  # get patch id of sample points
  point_id <- cbind(ID = 1:nrow(y), terra::extract(x = landscape_id, y = y))

  # rename df
  names(point_id) <- c("extract_id", "id")

  # check if length is identical if ids are provided
  if (!is.null(extract_id)) {

    if (length(extract_id) != nrow(point_id)) {

      if (verbose) {

        warning("Length of extract_id is not identical to length of y. Using 1...n as extract_id.",
                call. = FALSE)
      }

      extract_id <- seq_len(nrow(point_id))
    }
  }


  if (!is.null(extract_id)) {
    point_id[, 1] <- extract_id
  }

  point_id <- point_id[!duplicated(point_id), ]

  # calculate metrics
  # can we somehow calculate only the patches we actually want?
  metrics <- calculate_lsm(landscape,
                           what = metrics_list,
                           directions = directions,
                           verbose = verbose,
                           progress = progress, ...)

  # only patchs that contain a sample point
  extract_metrics <- merge(x = metrics, y = point_id,
                           by = "id",
                           all.x = FALSE, all.y = FALSE, sort = FALSE)

  # order cols
  extract_metrics <- extract_metrics[, c(names(metrics), "extract_id")]

  # order rows
  tibble::as_tibble(extract_metrics)
}

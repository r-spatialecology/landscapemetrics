#' metric_area
#'
#' @description Report area for each of the constituent values making up a matrix
#'
#' @details  Function reports the number of cells and the proportion of total cells made up by each unique class value
#'
#' @param landscape [\code{Raster* object}]
#' @param poi [\code{numerical}]\cr  Vector of numeric values indicating classes to be reported.
#'
#' @return List of tibbles
#'
#' @examples
#' library(NLMR)
#' landscape <- nlm_randomcluster(ncol = 30, nrow = 30,
#'                                p = 0.4, ai = c(0.25, 0.25, 0.5))
#'
#' metric_area(landscape)
#'
#' @aliases metric_area
#' @rdname metric_area
#'
#' @export
#'

metric_area <- function(landscape,
                        poi = NULL) {

  # Check function arguments ----
  checkmate::assert_class(landscape, "RasterLayer")

  if (is.null(poi)) {
    poi <- sort(unique(landscape@data@values))
  }

  freq_tib <- dplyr::tbl_df(raster::freq(landscape, digits = 20))

  if (length(poi) == 1) {
    area_poi <- freq_tib[freq_tib[, 1] == poi, 2]
    area_poi_perc <- area_poi / raster::ncell(landscape)
  } else {
    area_poi <- freq_tib[, 2]
    area_poi_perc <- dplyr::tbl_df(area_poi / raster::ncell(landscape))
  }

  area_poi$class <- poi
  area_poi_perc$class <- poi

  return(list(Total_Area = area_poi, Proportion_Area = area_poi_perc))
}


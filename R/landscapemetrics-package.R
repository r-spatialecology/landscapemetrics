#' @title landscapemetrics
#'
#' @description
#' Calculates landscape metrics for categorical landscape patterns in  a tidy workflow.
#' 'landscapemetrics' reimplements the most common metrics from 'FRAGSTATS' (<https://www.umass.edu/landeco/>)
#' and new ones from the current  literature on landscape metrics. This package
#' supports 'terra' SpatRaster objects  as input arguments. It further provides
#' utility functionn to visualize patches, select metrics and building blocks to
#' develop new metrics.
#'
#' @name landscapemetrics
#' @docType package
#' @useDynLib landscapemetrics
#' @importFrom Rcpp sourceCpp
#' @keywords internal
"_PACKAGE"

globalVariables(c("class_name",
                  "class.get_patches",
                  "count",
                  "crs",
                  "dist",
                  "extract_id",
                  "function_name",
                  "global",
                  "id",
                  "label",
                  "landscape",
                  "lsm_abbreviations_names",
                  "layer",
                  "layer2",
                  "level",
                  "metric",
                  "metric_new",
                  "metric_1",
                  "metric_2",
                  "minp",
                  "n",
                  "name",
                  "n_classes",
                  "OK",
                  "patch_id",
                  "raster_sample_plots",
                  "type",
                  "unpad_raster_value",
                  "value",
                  "values",
                  "x",
                  "x_centroid",
                  "y",
                  "y_centroid"))

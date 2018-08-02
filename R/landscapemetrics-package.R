#' @title landscapemetrics
#'
#' @description
#' Calculates landscape metrics for categorical landscape patterns in
#' a tidy workflow. 'landscapemetrics' reimplements the most common metrics from
#' FRAGSTATS and new ones from the current literature on landscape metrics.
#' This package supports raster spatial objects and takes
#' RasterLayer, RasterStacks, RasterBricks or lists of RasterLayer from the
#' 'raster' package as input arguments. It further provides utility functions
#' to visualize patches, select metrics and building blocks to develop new
#' metrics.
#'
#' @name landscapemetrics
#' @docType package
#' @useDynLib landscapemetrics
#' @importFrom Rcpp sourceCpp
# nocov start
"_PACKAGE"

globalVariables(c(".",
                  "area",
                  "layer",
                  "value_name",
                  "richness",
                  "landscape",
                  "landscape_stack",
                  "value",
                  "x",
                  "y",
                  "total_area",
                  "id",
                  "m",
                  "minp",
                  "n",
                  "count",
                  "dist",
                  "layer2",
                  "lsm_p_ncore",
                  "x1",
                  "x2",
                  "x3",
                  "x4",
                  "x_centroid",
                  "y1",
                  "y2",
                  "y3",
                  "y4",
                  "y_centroid",
                  "lsm_abbreviations_names",
                  "metric",
                  "metric_1",
                  "metric_2",
                  "values",
                  "level"))




# nocov end

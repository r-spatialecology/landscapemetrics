#' @title landscapemetrics
#'
#' @description
#' Calculates landscape metrics for categorical landscape patterns in a tidy workflow.
#' 'landscapemetrics' reimplements the most common metrics from 'FRAGSTATS' (<https://www.umass.edu/landeco/>)
#' and adds new ones from the current literature on landscape metrics. This package
#' supports 'terra' SpatRaster objects as input arguments. It further provides
#' utility functions to visualize patches, select metrics and building blocks to
#' develop new metrics.
#'
#' @name landscapemetrics
#' @docType package
#' @useDynLib landscapemetrics
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"

globalVariables(c("label", "metric_1", "metric_2", "value", "values", "x", "y"))

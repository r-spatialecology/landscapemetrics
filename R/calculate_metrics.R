#' Calculate metrics
#'
#' @description Calculate all metrics of level
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' calculate_metrics(landscape)
#' calculate_metrics(landscape_stack, what = "patch")
#'
#' @aliases calculate_metrics
#' @rdname calculate_metrics
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
calculate_metrics <- function(landscape, what) UseMethod("calculate_metrics")

#' @name calculate_metrics
#' @export
calculate_metrics.RasterLayer <- function(landscape, what = "all") {
    purrr::map_dfr(raster::as.list(landscape), calculate_metrics_internal,
                   what = what, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name calculate_metrics
#' @export
calculate_metrics.RasterStack <- function(landscape, what = "all") {
    purrr::map_dfr(raster::as.list(landscape), calculate_metrics_internal,
                   what = what, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name calculate_metrics
#' @export
calculate_metrics.RasterBrick <- function(landscape, what = "all") {
    purrr::map_dfr(raster::as.list(landscape), calculate_metrics_internal,
                   what = what, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name calculate_metrics
#' @export
calculate_metrics.list <- function(landscape, what = "all") {
    purrr::map_dfr(landscape, calculate_metrics_internal,
                   what = what, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

calculate_metrics_internal <- function(landscape, what = "all") {
    if(what == "all") {

        level <- "lsm"

        fcts <- lsf.str("package:landscapemetrics") %>%
            grep(pattern = level, x = ., value = TRUE) %>%
            grep(pattern = "\\.|calc", x = ., value = TRUE, invert = TRUE)
    }

    else if(what == "patch"){
        level <- "lsm_p"

        fcts <- lsf.str("package:landscapemetrics") %>%
            grep(pattern = level, x = ., value = TRUE) %>%
            grep(pattern = "\\.|calc", x = ., value = TRUE, invert = TRUE)
    }

    else if(what == "class"){
        level <- "lsm_c"

        fcts <- lsf.str("package:landscapemetrics") %>%
            grep(pattern = level, x = ., value = TRUE) %>%
            grep(pattern = "\\.|calc", x = ., value = TRUE, invert = TRUE)
    }

    else if(what == "landscape"){
        level <- "lsm_l"

        fcts <- lsf.str("package:landscapemetrics") %>%
            grep(pattern = level, x = ., value = TRUE) %>%
            grep(pattern = "\\.|calc", x = ., value = TRUE, invert = TRUE)
    }
}

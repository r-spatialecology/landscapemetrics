#' Calculate metrics
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "all", "patch", "class", "landscape". The default is "all".
#' @param ... Specific arguments for certain functions, if not provided they fall back to default.
#'
#' @return tibble
#'
#' @examples
#' lsm_calculate(landscape)
#' lsm_calculate(landscape_stack, what = "patch")
#'
#' @aliases lsm_calculate
#' @rdname lsm_calculate
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#' @export
lsm_calculate <- function(landscape, what, ...) UseMethod("lsm_calculate")

#' @name lsm_calculate
#' @export
lsm_calculate.RasterLayer <- function(landscape, what = "all", ...) {

    lsm_calculate_internal(landscape, what = what, ...)

}

#' @name lsm_calculate
#' @export
lsm_calculate.RasterStack <- function(landscape, what = "all", ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_calculate_internal,
                   what = what) %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_calculate
#' @export
lsm_calculate.RasterBrick <- function(landscape, what = "all", ...) {
    purrr::map_dfr(raster::as.list(landscape), lsm_calculate_internal,
                   what = what) %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_calculate
#' @export
lsm_calculate.list <- function(landscape, what = "all", ...) {
    purrr::map_dfr(landscape, lsm_calculate_internal,
                   what = what) %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_calculate_internal <- function(landscape, what = "all", ...) {
    # level <- "lsm"
    # lsf.str("package:landscapemetrics") %>%
    #     grep(pattern = level, x = ., value = TRUE) %>%
    #     grep(pattern = "\\.|calc", x = ., value = TRUE, invert = TRUE)


    if(what == "all") {


        result <- dplyr::bind_rows(
            lsm_p_area(landscape),
            lsm_p_perim(landscape),
            lsm_p_para(landscape),
            lsm_p_enn(landscape),

            lsm_c_ta(landscape),
            lsm_c_area_mn(landscape),
            lsm_c_area_cv(landscape),
            lsm_c_area_sd(landscape),
            lsm_c_pland(landscape),
            lsm_c_lpi(landscape),
            lsm_c_te(landscape),
            lsm_c_np(landscape),
            lsm_c_enn_mn(landscape),

            lsm_l_ta(landscape),
            lsm_l_area_mn(landscape),
            lsm_l_area_cv(landscape),
            lsm_l_area_sd(landscape),
            lsm_l_lpi(landscape),
            lsm_l_te(landscape),
            lsm_l_np(landscape),
            lsm_l_pr(landscape),
            lsm_l_prd(landscape),
            lsm_l_rpr(landscape, classes_max = ...),
            lsm_l_enn_mn(landscape),
            lsm_l_shei(landscape),
            lsm_l_shdi(landscape)
        )
    }

    if(what == "patch"){
        result <- dplyr::bind_rows(
            lsm_p_area(landscape),
            lsm_p_perim(landscape),
            lsm_p_para(landscape),
            lsm_p_enn(landscape)
        )
    }

    else if(what == "class"){
        result <- dplyr::bind_rows(
            lsm_c_ca(landscape),
            lsm_c_area_mn(landscape),
            lsm_c_area_cv(landscape),
            lsm_c_area_sd(landscape),
            lsm_c_pland(landscape),
            lsm_c_lpi(landscape),
            lsm_c_te(landscape),
            lsm_c_np(landscape),
            lsm_c_enn_mn(landscape)
        )

    }

    else if(what == "landscape"){
        result <- dplyr::bind_rows(
            lsm_l_ta(landscape),
            lsm_l_area_mn(landscape),
            lsm_l_area_cv(landscape),
            lsm_l_area_sd(landscape),
            lsm_l_lpi(landscape),
            lsm_l_te(landscape),
            lsm_l_np(landscape),
            lsm_l_pr(landscape),
            lsm_l_prd(landscape),
            lsm_l_rpr(landscape, classes_max = NULL),
            lsm_l_enn_mn(landscape),
            lsm_l_shei(landscape),
            lsm_l_shdi(landscape)
        )
    }
    return(result)
}

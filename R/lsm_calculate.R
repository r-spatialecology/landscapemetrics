#' Calculate metrics
#'
#' @description Calculate a selected group of metrics
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param what Selected level of metrics: either "all", "patch", "class",
#' "landscape". The default is "all". It is also possible to specifiy functions
#' as a vector of strings, e.g. `what = c("lsm_c_ca", "lsm_l_ta")`.
#' @param full_name Should the full names of all functions be included in the
#' tibble.
#' @param ... Specific arguments for certain functions, if not provided they
#' fall back to default.
#'
#' @return tibble
#'
#' @examples
#' \dontrun{
#' lsm_calculate(landscape)
#' lsm_calculate(landscape, what = "patch")
#' }
#'
#' @aliases lsm_calculate
#'
#' @rdname lsm_calculate
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_calculate <- function(landscape, what,
                          full_name = FALSE,  ...) UseMethod("lsm_calculate")

#' @name lsm_calculate
#' @export
lsm_calculate.RasterLayer <- function(landscape, what = "all", ...) {

    lsm_calculate_internal(landscape, what = what, ...)

}

#' @name lsm_calculate
#' @export
lsm_calculate.RasterStack <- function(landscape, what = "all", full_name = FALSE, ...) {

    purrr::map_dfr(raster::as.list(landscape, ...),
                   .f = lsm_calculate_internal,
                   what = what,
                   full_name = full_name,
                   .id = "layer2",
                   ...) %>%
        dplyr::mutate(layer = as.integer(layer2)) %>%
        dplyr::select(-layer2)
}

#' @name lsm_calculate
#' @export
lsm_calculate.RasterBrick <- function(landscape, what = "all", full_name = FALSE, ...) {
    purrr::map_dfr(raster::as.list(landscape, ...), lsm_calculate_internal,
                   what = what, full_name = full_name, ...) %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_calculate
#' @export
lsm_calculate.list <- function(landscape, what = "all", full_name = FALSE, ...) {
    purrr::map_dfr(landscape, lsm_calculate_internal,
                   what = what, full_name = full_name, ...) %>%
        dplyr::mutate(layer = as.integer(layer))
}

lsm_calculate_internal <- function(landscape, what, full_name = FALSE, ...) {

    if (any(what %in% c("all", "patch", "class", "landscape"))) {
        if (any(what == "all")) {
            result_all <- dplyr::bind_rows(
                # list.files(paste0(getwd(), "/R"), pattern = "_p_")
                lsm_p_area(landscape, ...),
                lsm_p_cai(landscape, ...),
                lsm_p_circle(landscape, ...),
                lsm_p_contig(landscape, ...),
                lsm_p_core(landscape, ...),
                lsm_p_enn(landscape, ...),
                lsm_p_frac(landscape, ...),
                lsm_p_gyrate(landscape, ...),
                lsm_p_ncore(landscape, ...),
                lsm_p_para(landscape, ...),
                lsm_p_perim(landscape, ...),
                # lsm_p_prox(landscape, ...),
                lsm_p_shape(landscape, ...),

                # list.files(paste0(getwd(), "/R"), pattern = "_c_")
                lsm_c_ai(landscape, ...),
                lsm_c_area_cv(landscape, ...),
                lsm_c_area_mn(landscape, ...),
                lsm_c_area_sd(landscape, ...),
                lsm_c_ca(landscape, ...),
                lsm_c_cai_cv(landscape, ...),
                lsm_c_cai_mn(landscape, ...),
                lsm_c_cai_sd(landscape, ...),
                lsm_c_circle_cv(landscape, ...),
                lsm_c_circle_mn(landscape, ...),
                lsm_c_circle_sd(landscape, ...),
                lsm_c_clumpy(landscape, ...),
                lsm_c_cohesion(landscape, ...),
                lsm_c_contig_cv(landscape, ...),
                lsm_c_contig_mn(landscape, ...),
                lsm_c_contig_sd(landscape, ...),
                lsm_c_core_cv(landscape, ...),
                lsm_c_core_mn(landscape, ...),
                lsm_c_core_sd(landscape, ...),
                lsm_c_cpland(landscape, ...),
                lsm_c_dcad(landscape, ...),
                lsm_c_dcore_cv(landscape, ...),
                lsm_c_dcore_mn(landscape, ...),
                lsm_c_dcore_sd(landscape, ...),
                lsm_c_division(landscape, ...),
                lsm_c_ed(landscape, ...),
                lsm_c_enn_cv(landscape, ...),
                lsm_c_enn_mn(landscape, ...),
                lsm_c_enn_sd(landscape, ...),
                lsm_c_frac_cv(landscape, ...),
                lsm_c_frac_mn(landscape, ...),
                lsm_c_frac_sd(landscape, ...),
                lsm_c_gyrate_cv(landscape, ...),
                lsm_c_gyrate_mn(landscape, ...),
                lsm_c_gyrate_sd(landscape, ...),
                # # lsm_c_iji(landscape, ...),
                lsm_c_lpi(landscape, ...),
                lsm_c_lsi(landscape, ...),
                lsm_c_mesh(landscape, ...),
                lsm_c_ndca(landscape, ...),
                # lsm_c_nlsi(landscape, ...),
                lsm_c_np(landscape, ...),
                lsm_c_pafrac(landscape, ...),
                lsm_c_para_cv(landscape, ...),
                lsm_c_para_mn(landscape, ...),
                lsm_c_para_sd(landscape, ...),
                lsm_c_pd(landscape, ...),
                lsm_c_pladj(landscape, ...),
                lsm_c_pland(landscape, ...),
                lsm_c_shape_cv(landscape, ...),
                lsm_c_shape_mn(landscape, ...),
                lsm_c_shape_sd(landscape, ...),
                lsm_c_split(landscape, ...),
                lsm_c_tca(landscape, ...),
                lsm_c_te(landscape, ...),

                # list.files(paste0(getwd(), "/R"), pattern = "_l_")
                lsm_l_ai(landscape, ...),
                lsm_l_area_cv(landscape, ...),
                lsm_l_area_mn(landscape, ...),
                lsm_l_area_sd(landscape, ...),
                lsm_l_cai_cv(landscape, ...),
                lsm_l_cai_mn(landscape, ...),
                lsm_l_cai_sd(landscape, ...),
                lsm_l_circle_cv(landscape, ...),
                lsm_l_circle_mn(landscape, ...),
                lsm_l_circle_sd(landscape, ...),
                lsm_l_cohesion(landscape, ...),
                lsm_l_condent(landscape, ...),
                lsm_l_contig_cv(landscape, ...),
                lsm_l_contig_mn(landscape, ...),
                lsm_l_contig_sd(landscape, ...),
                lsm_l_core_cv(landscape, ...),
                lsm_l_core_mn(landscape, ...),
                lsm_l_core_sd(landscape, ...),
                lsm_l_dcad(landscape, ...),
                lsm_l_dcore_cv(landscape, ...),
                lsm_l_dcore_mn(landscape, ...),
                lsm_l_dcore_sd(landscape, ...),
                lsm_l_division(landscape, ...),
                lsm_l_ed(landscape, ...),
                lsm_l_enn_cv(landscape, ...),
                lsm_l_enn_mn(landscape, ...),
                lsm_l_enn_sd(landscape, ...),
                lsm_l_ent(landscape, ...),
                lsm_l_frac_cv(landscape, ...),
                lsm_l_frac_mn(landscape, ...),
                lsm_l_frac_sd(landscape, ...),
                lsm_l_gyrate_cv(landscape, ...),
                lsm_l_gyrate_mn(landscape, ...),
                lsm_l_gyrate_sd(landscape, ...),
                lsm_l_joinent(landscape, ...),
                lsm_l_lpi(landscape, ...),
                lsm_l_lsi(landscape, ...),
                lsm_l_mesh(landscape, ...),
                lsm_l_msidi(landscape, ...),
                lsm_l_msiei(landscape, ...),
                lsm_l_mutinf(landscape, ...),
                lsm_l_ndca(landscape, ...),
                lsm_l_np(landscape, ...),
                lsm_l_pafrac(landscape, ...),
                lsm_l_para_cv(landscape, ...),
                lsm_l_para_mn(landscape, ...),
                lsm_l_para_sd(landscape, ...),
                lsm_l_pd(landscape, ...),
                lsm_l_pladj(landscape, ...),
                lsm_l_pr(landscape, ...),
                lsm_l_prd(landscape, ...),
                lsm_l_rpr(landscape, ...),
                lsm_l_shape_cv(landscape, ...),
                lsm_l_shape_mn(landscape, ...),
                lsm_l_shape_sd(landscape, ...),
                lsm_l_shdi(landscape, ...),
                lsm_l_shei(landscape, ...),
                lsm_l_sidi(landscape, ...),
                lsm_l_siei(landscape, ...),
                lsm_l_split(landscape, ...),
                lsm_l_ta(landscape, ...),
                lsm_l_tca(landscape, ...),
                lsm_l_te(landscape, ...)
            )
        }

        if (any(what == "patch")) {
            result_patch <- dplyr::bind_rows(
                lsm_p_area(landscape, ...),
                lsm_p_cai(landscape, ...),
                lsm_p_circle(landscape, ...),
                lsm_p_contig(landscape, ...),
                lsm_p_core(landscape, ...),
                lsm_p_enn(landscape, ...),
                lsm_p_frac(landscape, ...),
                lsm_p_gyrate(landscape, ...),
                lsm_p_ncore(landscape, ...),
                lsm_p_para(landscape, ...),
                lsm_p_perim(landscape, ...),
                # lsm_p_prox(landscape, ...),
                lsm_p_shape(landscape, ...)
            )
        }

        if (any(what == "class")) {
            result_class <- dplyr::bind_rows(
                lsm_c_ai(landscape, ...),
                lsm_c_area_cv(landscape, ...),
                lsm_c_area_mn(landscape, ...),
                lsm_c_area_sd(landscape, ...),
                lsm_c_ca(landscape, ...),
                lsm_c_cai_cv(landscape, ...),
                lsm_c_cai_mn(landscape, ...),
                lsm_c_cai_sd(landscape, ...),
                lsm_c_circle_cv(landscape, ...),
                lsm_c_circle_mn(landscape, ...),
                lsm_c_circle_sd(landscape, ...),
                lsm_c_clumpy(landscape, ...),
                lsm_c_cohesion(landscape, ...),
                lsm_c_contig_cv(landscape, ...),
                lsm_c_contig_mn(landscape, ...),
                lsm_c_contig_sd(landscape, ...),
                lsm_c_core_cv(landscape, ...),
                lsm_c_core_mn(landscape, ...),
                lsm_c_core_sd(landscape, ...),
                lsm_c_cpland(landscape, ...),
                lsm_c_dcad(landscape, ...),
                lsm_c_dcore_cv(landscape, ...),
                lsm_c_dcore_mn(landscape, ...),
                lsm_c_dcore_sd(landscape, ...),
                lsm_c_division(landscape, ...),
                lsm_c_ed(landscape, ...),
                lsm_c_enn_cv(landscape, ...),
                lsm_c_enn_mn(landscape, ...),
                lsm_c_enn_sd(landscape, ...),
                lsm_c_frac_cv(landscape, ...),
                lsm_c_frac_mn(landscape, ...),
                lsm_c_frac_sd(landscape, ...),
                lsm_c_gyrate_cv(landscape, ...),
                lsm_c_gyrate_mn(landscape, ...),
                lsm_c_gyrate_sd(landscape, ...),
                # # lsm_c_iji(landscape, ...),
                lsm_c_lpi(landscape, ...),
                lsm_c_lsi(landscape, ...),
                lsm_c_mesh(landscape, ...),
                lsm_c_ndca(landscape, ...),
                # lsm_c_nlsi(landscape, ...),
                lsm_c_np(landscape, ...),
                lsm_c_pafrac(landscape, ...),
                lsm_c_para_cv(landscape, ...),
                lsm_c_para_mn(landscape, ...),
                lsm_c_para_sd(landscape, ...),
                lsm_c_pd(landscape, ...),
                lsm_c_pladj(landscape, ...),
                lsm_c_pland(landscape, ...),
                lsm_c_shape_cv(landscape, ...),
                lsm_c_shape_mn(landscape, ...),
                lsm_c_shape_sd(landscape, ...),
                lsm_c_split(landscape, ...),
                lsm_c_tca(landscape, ...),
                lsm_c_te(landscape, ...)
            )
        }

        if (any(what == "landscape")) {
            result_landscape <- dplyr::bind_rows(
                lsm_l_ai(landscape, ...),
                lsm_l_area_cv(landscape, ...),
                lsm_l_area_mn(landscape, ...),
                lsm_l_area_sd(landscape, ...),
                lsm_l_cai_cv(landscape, ...),
                lsm_l_cai_mn(landscape, ...),
                lsm_l_cai_sd(landscape, ...),
                lsm_l_circle_cv(landscape, ...),
                lsm_l_circle_mn(landscape, ...),
                lsm_l_circle_sd(landscape, ...),
                lsm_l_cohesion(landscape, ...),
                lsm_l_condent(landscape, ...),
                lsm_l_contig_cv(landscape, ...),
                lsm_l_contig_mn(landscape, ...),
                lsm_l_contig_sd(landscape, ...),
                lsm_l_core_cv(landscape, ...),
                lsm_l_core_mn(landscape, ...),
                lsm_l_core_sd(landscape, ...),
                lsm_l_dcad(landscape, ...),
                lsm_l_dcore_cv(landscape, ...),
                lsm_l_dcore_mn(landscape, ...),
                lsm_l_dcore_sd(landscape, ...),
                lsm_l_division(landscape, ...),
                lsm_l_ed(landscape, ...),
                lsm_l_enn_cv(landscape, ...),
                lsm_l_enn_mn(landscape, ...),
                lsm_l_enn_sd(landscape, ...),
                lsm_l_ent(landscape, ...),
                lsm_l_frac_cv(landscape, ...),
                lsm_l_frac_mn(landscape, ...),
                lsm_l_frac_sd(landscape, ...),
                lsm_l_gyrate_cv(landscape, ...),
                lsm_l_gyrate_mn(landscape, ...),
                lsm_l_gyrate_sd(landscape, ...),
                lsm_l_joinent(landscape, ...),
                lsm_l_lpi(landscape, ...),
                lsm_l_lsi(landscape, ...),
                lsm_l_mesh(landscape, ...),
                lsm_l_msidi(landscape, ...),
                lsm_l_msiei(landscape, ...),
                lsm_l_mutinf(landscape, ...),
                lsm_l_ndca(landscape, ...),
                lsm_l_np(landscape, ...),
                lsm_l_pafrac(landscape, ...),
                lsm_l_para_cv(landscape, ...),
                lsm_l_para_mn(landscape, ...),
                lsm_l_para_sd(landscape, ...),
                lsm_l_pd(landscape, ...),
                lsm_l_pladj(landscape, ...),
                lsm_l_pr(landscape, ...),
                lsm_l_prd(landscape, ...),
                lsm_l_rpr(landscape, ...),
                lsm_l_shape_cv(landscape, ...),
                lsm_l_shape_mn(landscape, ...),
                lsm_l_shape_sd(landscape, ...),
                lsm_l_shdi(landscape, ...),
                lsm_l_shei(landscape, ...),
                lsm_l_sidi(landscape, ...),
                lsm_l_siei(landscape, ...),
                lsm_l_split(landscape, ...),
                lsm_l_ta(landscape, ...),
                lsm_l_tca(landscape, ...),
                lsm_l_te(landscape, ...)
            )
        }

        if(!exists("result_all")){result_all <- tibble::tibble()}
        if(!exists("result_patch")){result_patch <- tibble::tibble()}
        if(!exists("result_class")){result_class <- tibble::tibble()}
        if(!exists("result_landscape")){result_landscape <- tibble::tibble()}

        result_level <- dplyr::bind_rows(result_all,
                                   result_patch,
                                   result_class,
                                   result_landscape)
    }

    if (any(!(what %in% c("all", "patch", "class", "landscape")))) {

        what <- what[!(what %in% c("all", "patch", "class", "landscape"))]

        result_metrics <- purrr::map_dfr(what, function(current_function) {
            foo <- match.fun(current_function)
            tryCatch({foo(landscape,  ...)},
                     error = function(e) foo(landscape))

        })
    }

    if(!exists("result_level")){result_level <- tibble::tibble()}
    if(!exists("result_metrics")){result_metrics <- tibble::tibble()}

    result <- dplyr::bind_rows(result_level,
                               result_metrics)

    if(full_name == TRUE){
        result <- dplyr::left_join(x = result,
                                   y = lsm_abbreviations_names,
                                   by = "metric")
    }

    return(result)
}

#' PERIM (patch level)
#'
#' @description Perimeter (Area and edge metric))
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @details
#' \deqn{PERIM = p_{ij}}
#' where \eqn{p_{ij}} is the perimeter in meters.
#'
#' PERIM is an 'Area and edge metric'. It equals the perimeter of the patch including
#' also the edge to the landscape boundary. The metric describes patch area (larger perimeter
#' for larger patches), but also patch shape (large perimeter for irregular shapes).
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{PERIM > 0}
#' \subsection{Behaviour}{Increases, without limit, as patch size and complexity increases.}
#'
#' @return tibble
#'
#' @examples
#' lsm_p_perim(landscape)
#'
#' @aliases lsm_p_perim
#' @rdname lsm_p_perim
#'
#' @references
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis
#' Program for Categorical and Continuous Maps. Computer software program produced by
#' the authors at the University of Massachusetts, Amherst. Available at the following
#' web site: http://www.umass.edu/landeco/research/fragstats/fragstats.html
#'
#' @export
lsm_p_perim <- function(landscape) UseMethod("lsm_p_perim")

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterLayer <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_perim_calc <- function(landscape) {
    landscape_padded <- pad_raster(landscape, pad_raster_value = NA)

    landscape_labelled <- cclabel(landscape_padded)

    perimeter_patch <-
        purrr::map_dfr(landscape_labelled, function(patches_class) {

            target_na <- raster::Which(is.na(patches_class), cells = TRUE)

            raster::values(patches_class)[target_na] <- -999

            neighbour_matrix <- rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                            directions = as.matrix(4))

            neighbour_matrix <- neighbour_matrix[1 ,2:ncol(neighbour_matrix)]

            perimeter_patch_ij <-
                neighbour_matrix * raster::res(patches_class)[[1]]

            class_name <- patches_class %>%
                names() %>%
                sub("Class_", "", .)

            tibble::tibble(class = class_name,
                           value = perimeter_patch_ij)
        })

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(seq_len(nrow(
            perimeter_patch
        ))),
        metric = "perim",
        value = as.double(perimeter_patch$value)
    )
}

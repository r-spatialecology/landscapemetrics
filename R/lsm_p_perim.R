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
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterStack <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterBrick <- function(landscape) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.list <- function(landscape) {
    purrr::map_dfr(landscape, lsm_p_perim_calc, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_perim_calc <- function(landscape){

    perimeter_patch <- landscape %>%
        pad_raster(pad_raster_value = NA) %>%
        cclabel() %>%
        purrr::map_dfr(function(patches_class) {
            patches_class %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(patch_id) {
                    patches_class[patches_class != patch_id | is.na(patches_class)] <- -999

                    adjacent_cells <- raster::adjacent(x = patches_class,
                                                       cells = seq_len(raster::ncell(patches_class)),
                                                       directions = 4,
                                                       pairs = TRUE)

                    neighbour_matrix <- table(patches_class[adjacent_cells[,1]],
                                              patches_class[adjacent_cells[,2]])

                    perimeter_patch_ij <- neighbour_matrix[2:ncol(neighbour_matrix),1] *
                        raster::res(patches_class)[[1]]

                    class_name <- patches_class %>%
                        names() %>%
                        sub("Class_", "", .)

                    tibble::tibble(
                        class = class_name,
                        value = perimeter_patch_ij
                    )
                })
        })

    tibble::tibble(
        level = "patch",
        class = as.integer(perimeter_patch$class),
        id = as.integer(seq_len(nrow(perimeter_patch))),
        metric = "perimeter",
        value = as.double(perimeter_patch$value)
    )
}

#' PERIM (patch level)
#'
#' @description Perimeter (Area and edge metric))
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#'
#' @details
#' \deqn{PERIM = p_{ij}}
#' where \eqn{p_{ij}} is the perimeter in meters.
#'
#' PERIM is an 'Area and edge metric'. It equals the perimeter of the patch
#' including also the edge to the landscape boundary. The metric describes
#' patch area (larger perimeter for larger patches), but also patch shape
#' (large perimeter for irregular shapes).
#'
#' \subsection{Units}{Meters}
#' \subsection{Range}{PERIM > 0}
#' \subsection{Behaviour}{Increases, without limit, as patch size and
#' complexity increases.}
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
lsm_p_perim <- function(landscape, directions) UseMethod("lsm_p_perim")

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterLayer <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterStack <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.RasterBrick <- function(landscape, directions = 8) {
    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_perim
#' @export
lsm_p_perim.stars <- function(landscape, directions = 8) {

    landscape <- methods::as(landscape, "Raster")

    purrr::map_dfr(raster::as.list(landscape),
                   lsm_p_perim_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}
#' @name lsm_p_perim
#' @export
lsm_p_perim.list <- function(landscape, directions = 8) {
    purrr::map_dfr(landscape,
                   lsm_p_perim_calc,
                   directions = directions,
                   .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_perim_calc <- function(landscape, directions) {

    if(!isTRUE(raster::res(landscape)[[1]] == raster::res(landscape)[[2]])){
       top_bottom_matrix <- matrix(c(NA, NA, NA,
                                      1,  0, 1,
                                     NA, NA, NA), 3, 3, byrow = TRUE)

        left_right_matrix <- matrix(c(NA, 1, NA,
                                      NA, 0, NA,
                                      NA, 1, NA), 3, 3, byrow = TRUE)
    }

    landscape_labeled <- get_patches(landscape, directions = directions)

    perimeter_patch <-
        purrr::map_dfr(landscape_labeled, function(patches_class) {

            patches_class <- pad_raster(patches_class, pad_raster_value = NA)

            target_na <- raster::Which(is.na(patches_class), cells = TRUE)

            raster::values(patches_class)[target_na] <- -999

            if(isTRUE(raster::res(landscape)[[1]] == raster::res(landscape)[[2]])) {

                neighbour_matrix <-
                    rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                directions = as.matrix(4))
                neighbour_matrix <-
                    neighbour_matrix[1 ,2:ncol(neighbour_matrix)]

                perimeter_patch_ij <-
                    neighbour_matrix * raster::res(patches_class)[[1]]
            }

            else{

                left_right_neighbours <-
                    rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                directions = as.matrix(left_right_matrix))

                perimeter_patch_ij_left_right <-
                    left_right_neighbours[1 ,2:ncol(left_right_neighbours)] *
                    raster::res(patches_class)[[1]]

                top_bottom_neighbours <-
                    rcpp_get_coocurrence_matrix(raster::as.matrix(patches_class),
                                                directions = as.matrix(top_bottom_matrix))

                perimeter_patch_ij_top_bottom <-
                    top_bottom_neighbours[1 ,2:ncol(top_bottom_neighbours)] *
                    raster::res(patches_class)[[2]]

                perimeter_patch_ij <- perimeter_patch_ij_top_bottom +
                    perimeter_patch_ij_left_right
            }

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

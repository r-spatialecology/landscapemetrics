#'  Number of core areas  (patch level)
#'
#' @description Number of core cells
#'
#' @param landscape Raster* Layer, Stack, Brick or a list of rasterLayers.
#'
#' @return tibble
#'
#' @examples
#' lsm_p_ncore(landscape)
#' lsm_p_ncore(landscape_stack)
#'
#' @aliases lsm_p_ncore
#' @rdname lsm_p_ncore
#'
#' @references
#' McGarigal, K., and B. J. Marks. 1995. FRAGSTATS: spatial pattern analysis
#' program for quantifying landscape structure. USDA For. Serv. Gen. Tech. Rep.
#'  PNW-351.
#'
#' @export
lsm_p_ncore <- function(landscape, directions) UseMethod("lsm_p_ncore")

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterLayer <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))
}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterStack <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.RasterBrick <- function(landscape, directions = 4) {
    purrr::map_dfr(raster::as.list(landscape), lsm_p_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

#' @name lsm_p_ncore
#' @export
lsm_p_ncore.list <- function(landscape, directions = 4) {
    purrr::map_dfr(landscape, lsm_p_ncore_calc,
                   directions = directions, .id = "layer") %>%
        dplyr::mutate(layer = as.integer(layer))

}

lsm_p_ncore_calc <- function(landscape, directions){

    core_class <- landscape %>%
        cclabel() %>%
        unname() %>%
        purrr::map_dfr(function(x) {
            x %>%
                raster::values() %>%
                stats::na.omit() %>%
                unique() %>%
                sort() %>%
                purrr::map_dfr(function(y) {
                    landscape_patch <- x
                    landscape_patch[landscape_patch != y | is.na(landscape_patch)] <- -999

                    core_cells <- raster::Which(landscape_patch == y, cells = T) %>%
                        purrr::map_dbl(function(z) {
                        adjacent_cells <- raster::adjacent(landscape_patch,
                                                           cells = z,
                                                           directions = directions, # Use matrix for direction
                                                           pairs = FALSE)
                        ifelse(all(landscape_patch[adjacent_cells] == y), z, NA)
                        }) %>%
                        na.omit()

                    landscape_patch[!1:ncell(landscape_patch) %in% core_cells] <- NA

                    core_patch_n <- landscape_patch %>% # PROBLEM IF ALL NA
                        lsm_l_np() %>%
                        dplyr::pull(value)

                    tibble::tibble(
                        id = NA,
                        value = core_patch_n
                    )
                })
        }, .id = "class")

    tibble::tibble(
        level = "patch",
        class = as.integer(core_class$class),
        id = seq_len(nrow(core_class)),
        metric = "number of core area",
        value = core_class$value
    )
}

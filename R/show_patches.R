#' Plot connected labeld
#'
#' @description xxx
#'
#' @param landscape *Raster object
#'
#' @details xxx
#'
#' @return List with RasterLayer/RasterBrick
#'
#' @examples
#' show_patches(landscape)
#' show_patches(landscape_stack)
#'
#' @aliases show_patches
#' @rdname show_patches
#'
#' @export
show_patches <- function(landscape)  UseMethod("show_patches")

#' @name show_patches
#' @export
show_patches.RasterLayer <- function(landscape) {
    show_patches_intern(landscape)
}

#' @name show_patches
#' @export
show_patches.RasterStack <- function(landscape) {
    purrr::map(raster::as.list(landscape), show_patches_intern)
}

#' @name show_patches
#' @export
show_patches.RasterBrick <- function(landscape) {
    purrr::map(raster::as.list(landscape), show_patches_intern)
}

#' @name show_patches
#' @export
show_patches.list <- function(landscape) {
    purrr::map(landscape, show_patches_intern)
}

show_patches_intern <- function(landscape) {

    labeled_landscape <- landscape %>%
        cclabel()

    for(i in seq_len(length(labeled_landscape) - 1)){
        max_patch_id <- labeled_landscape[[i]] %>%
            raster::values() %>%
            max(na.rm = TRUE)

        labeled_landscape[[i + 1]] <- labeled_landscape[[i + 1]] + max_patch_id
    }

    labeled_landscape %>%
        raster::stack() %>%
        sum(na.rm = TRUE) %>%
        raster::as.data.frame(xy = TRUE) %>%
        ggplot2::ggplot() +
            ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = layer)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = layer), colour = "white") +
            ggplot2::coord_equal() +
            ggplot2::theme_void() +
            ggplot2::guides(fill = FALSE) +
            ggplot2::scale_fill_viridis_c()
}

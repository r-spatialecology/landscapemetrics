#' Show patches
#'
#' @description Show patches
#'
#' @param landscape *Raster object
#'
#' @details The functions plots the landscape with the patches labelled with the
#' corresponding patch id.
#'
#' @return ggplot
#'
#' @examples
#' show_patches(landscape)
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

    landscape_labelled <- cclabel(landscape)

    for(i in seq_len(length(landscape_labelled) - 1)){
        max_patch_id <- landscape_labelled[[i]] %>%
            raster::values() %>%
            max(na.rm = TRUE)

        landscape_labelled[[i + 1]] <- landscape_labelled[[i + 1]] + max_patch_id
    }

    landscape_labelled_stack <- landscape_labelled %>%
        raster::stack() %>%
        sum(na.rm = TRUE) %>%
        raster::as.data.frame(xy = TRUE)

    plot <- ggplot2::ggplot(landscape_labelled_stack) +
        ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = layer)) +
        ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = layer),
                           colour = "white") +
        ggplot2::coord_equal() +
        ggplot2::theme_void() +
        ggplot2::guides(fill = FALSE) +
        ggplot2::scale_fill_gradientn(
            colours = c(
                "#5F4690",
                "#1D6996",
                "#38A6A5",
                "#0F8554",
                "#73AF48",
                "#EDAD08",
                "#E17C05",
                "#CC503E",
                "#94346E",
                "#6F4070",
                "#994E95",
                "#666666"
                )) +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       axis.ticks.length = ggplot2::unit(0, "lines"),
                       legend.position = "none",
                       panel.background = ggplot2::element_blank(),
                       panel.border = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.spacing = ggplot2::unit(0, "lines"),
                       plot.background = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(-1, -1, -1.5, -1.5), "lines")) +
        ggplot2::labs(x = NULL, y = NULL)

    return(plot)
}

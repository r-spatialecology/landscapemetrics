#' Show patches
#'
#' @description Show patches
#'
#' @param landscape *Raster object
#' @param what How to show the labeled patches: "global" (single map), "all" (every class as facet), or a vector with the specific classes one wants to show (every selected class as facet).
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param labels Logical flag indicating whether to print or not to print patch labels.
#' @param nrow,ncol Number of rows and columns for the facet.
#'
#' @details The functions plots the landscape with the patches labeled with the
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
show_patches <- function(landscape, what, directions, labels, nrow, ncol)  UseMethod("show_patches")

#' @name show_patches
#' @export
show_patches.RasterLayer <- function(landscape,
                                     what = "global",
                                     directions = 8,
                                     labels = TRUE,
                                     nrow = NULL,
                                     ncol = NULL) {

    show_patches_intern(landscape,
                        what = what,
                        directions = directions,
                        labels = labels,
                        nrow = nrow,
                        ncol = ncol)
}

#' @name show_patches
#' @export
show_patches.RasterStack <- function(landscape,
                                     what = "global",
                                     directions = 8,
                                     labels = TRUE,
                                     nrow = NULL,
                                     ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_patches_intern,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_patches
#' @export
show_patches.RasterBrick <- function(landscape,
                                     what = "global",
                                     directions = 8,
                                     labels = TRUE,
                                     nrow = NULL,
                                     ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_patches_intern,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_patches
#' @export
show_patches.stars <- function(landscape,
                               what = "global",
                               directions = 8,
                               labels = TRUE,
                               nrow = NULL,
                               ncol = NULL) {

    landscape <- methods::as(landscape, "Raster")

    lapply(X = raster::as.list(landscape),
           FUN = show_patches_intern,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_patches
#' @export
show_patches.list <- function(landscape,
                              what = "global",
                              directions = 8,
                              labels = TRUE,
                              nrow = NULL,
                              ncol = NULL) {

    lapply(X = landscape,
           FUN = show_patches_intern,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

show_patches_intern <- function(landscape, what, directions, labels, nrow, ncol) {

    if(any(!(what %in% c("all", "global")))){
        if (!any(what %in% raster::unique(landscape))){
            stop("what must at least contain one value of a class contained in the landscape.", call. = FALSE)
        }
    }

    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)){

        max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }

    if (any(what == "global")) {

        landscape_labeled_stack <- raster::as.data.frame(sum(raster::stack(landscape_labeled),
                                                             na.rm = TRUE),
                                                         xy = TRUE)

        names(landscape_labeled_stack) <- c("x", "y", "values")

        landscape_labeled_stack <- dplyr::mutate(landscape_labeled_stack,
                                                 values = replace(values, values == 0, NA))

        if (isTRUE(labels)) {
            landscape_labeled_stack$labels <- landscape_labeled_stack$values
        }

        if (!isTRUE(labels)) {
            landscape_labeled_stack$labels <- NA
        }

        plot <- ggplot2::ggplot(landscape_labeled_stack) +
            ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = values)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = labels),
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
                    "#994E95"
                ),
                na.value = "grey75"
            ) +
            ggplot2::theme(
                axis.title = ggplot2::element_blank(),
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
                plot.margin = ggplot2::unit(c(-1,-1,-1.5,-1.5), "lines")
            ) +
            ggplot2::labs(x = NULL, y = NULL)

    }

    if (any(what != "global")) {

        patches_tibble <- lapply(X = landscape_labeled, FUN = function(x){
            names(x) <- "value"
            x <- raster::as.data.frame(x, xy = TRUE)
            return(x)}
        )

        patches_tibble <- dplyr::bind_rows(patches_tibble, .id = "class")

        if (any(!(what %in% c("all", "global")))){
            patches_tibble <- dplyr::filter(patches_tibble, class %in% what)
        }

        if (isTRUE(labels)) {
            patches_tibble$labels <- patches_tibble$value
        }

        if (!isTRUE(labels)) {
            patches_tibble$labels <- NA
        }

        plot <- ggplot2::ggplot(patches_tibble, ggplot2::aes(x, y)) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes(fill = value)) +
            ggplot2::geom_text(ggplot2::aes(label = labels),
                               colour = "white")  +
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
                    "#994E95"
                ),
                na.value = "grey75"
            ) +
            ggplot2::facet_wrap(~class, nrow = nrow, ncol = ncol) +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            ggplot2::guides(fill = FALSE) +
            ggplot2::labs(titel = NULL, x = NULL, y = NULL) +
            ggplot2::theme(
                axis.title  = ggplot2::element_blank(),
                axis.ticks  = ggplot2::element_blank(),
                axis.text   = ggplot2::element_blank(),
                panel.grid  = ggplot2::element_blank(),
                axis.line   = ggplot2::element_blank(),
                strip.background = ggplot2::element_rect(fill = "grey80"),
                strip.text = ggplot2::element_text(hjust  = 0),
                plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines"))


    }

    suppressWarnings(print(plot))
}

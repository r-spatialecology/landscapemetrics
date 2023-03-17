#' Show patches
#'
#' @description Show patches
#'
#' @param landscape *Raster object
#' @param class How to show the labeled patches: "global" (single map), "all" (every class as facet), or a vector with the specific classes one wants to show (every selected class as facet).
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
#' landscape <- terra::rast(landscape)
#'
#' show_patches(landscape)
#' show_patches(landscape, class = c(1, 2))
#' show_patches(landscape, class = 3, labels = FALSE)
#'
#' @aliases show_patches
#' @rdname show_patches
#'
#' @export
show_patches <- function(landscape, class = "global", directions = 8,
                         labels = FALSE, nrow = NULL, ncol = NULL) {

    landscape <- landscape_as_list(landscape)

    result <- lapply(X = landscape,
                     FUN = show_patches_internal,
                     class = class,
                     directions = directions,
                     labels = labels,
                     nrow = nrow,
                     ncol = ncol)

    names(result) <- paste0("layer_", 1:length(result))

    return(result)
}

show_patches_internal <- function(landscape, class, directions, labels, nrow, ncol) {

    if (any(!(class %in% c("all", "global")))) {
        if (!any(class %in% unique(terra::values(landscape, mat = FALSE)))) {
            stop("'class' must at least contain one value of a class contained in the landscape.", call. = FALSE)
        }
    }

    if (length(class) > 1 & any(class %in% c("all", "global"))) {
        warning("'global' and 'all' can't be combined with any other class-argument.", call. = FALSE)
    }

    landscape_labeled <- get_patches(landscape, directions = directions)[[1]]

    if (any(class == "global")) {

        patches_tibble <- terra::as.data.frame(sum(terra::rast(landscape_labeled),
                                                    na.rm = TRUE),
                                                xy = TRUE)

        names(patches_tibble) <- c("x", "y", "value")

        patches_tibble$value <- replace(patches_tibble$value, patches_tibble$value == 0, NA)

        if (labels) {
            patches_tibble$labels <- patches_tibble$value
        }

        if (!labels) {
            patches_tibble$labels <- NA
        }

        patches_tibble$class <- "global"
    }

    if (any(class != "global")) {

        patches_tibble <- lapply(X = seq_along(landscape_labeled), FUN = function(i){
            names(landscape_labeled[[i]]) <- "value"
            x <- terra::as.data.frame(landscape_labeled[[i]], xy = TRUE)
            x$class <- names(landscape_labeled[i])
            return(x)}
        )

        patches_tibble <- do.call(rbind, patches_tibble)

        if (any(!(class %in% c("all", "global")))) {
            class_index <- which(patches_tibble$class %in% paste0("class_", class))
            patches_tibble <- patches_tibble[class_index, ]
        }

        if (labels) {
            patches_tibble$labels <- patches_tibble$value
        }

        if (!labels) {
            patches_tibble$labels <- NA
        }
    }

    plot <- ggplot2::ggplot(patches_tibble, ggplot2::aes(x, y)) +
        ggplot2::coord_fixed() +
        ggplot2::geom_raster(ggplot2::aes(fill = value)) +
        ggplot2::geom_text(ggplot2::aes(label = labels),
                           colour = "white", na.rm = TRUE)  +
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
            na.value = "grey85"
        ) +
        ggplot2::facet_wrap(~class, nrow = nrow, ncol = ncol) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::guides(fill = "none") +
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

    return(plot)
}

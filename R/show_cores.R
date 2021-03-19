#' Show core area
#'
#' @description Show core area
#'
#' @param landscape Raster object
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param class How to show the core area: "global" (single map), "all" (every class as facet), or a vector with the specific classes one wants to show (every selected class as facet).
#' @param consider_boundary Logical if cells that only neighbour the landscape
#' boundary should be considered as core.
#' @param labels Logical flag indicating whether to print or not to print core labels.
#' boundary should be considered as core
#' @param nrow,ncol Number of rows and columns for the facet.
#' @param edge_depth Distance (in cells) a cell has the be away from the patch
#' edge to be considered as core cell
#'
#' @details The functions plots the core area of patches labeled with the
#' corresponding patch id. The edges are the grey cells surrounding the patches and are always shown.
#'
#' @return ggplot
#'
#' @examples
#' # show "global" core area
#' show_cores(landscape, class = "global", labels = FALSE)
#'
#' # show the core area of every class as facet
#' show_cores(landscape, class = "all", labels = FALSE)
#'
#' # show only the core area of class 1 and 3
#' show_cores(landscape, class = c(1, 3), labels = TRUE)
#'
#' @aliases show_cores
#' @rdname show_cores
#'
#' @export
show_cores <- function(landscape,
                       directions = 8,
                       class = "all",
                       labels = FALSE,
                       nrow = NULL,
                       ncol = NULL,
                       consider_boundary = TRUE,
                       edge_depth = 1) {

    landscape <- landscape_as_list(landscape)

    lapply(X = landscape,
           FUN = show_cores_internal,
           directions = directions,
           class = class,
           labels = labels,
           nrow = nrow,
           ncol = ncol,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth)

}

show_cores_internal <- function(landscape, directions, class, labels, nrow, ncol,
                                consider_boundary, edge_depth ) {

    if (any(!(class %in% c("all", "global")))) {

        if (!any(class %in% raster::unique(landscape))) {

            stop("class must at least contain one value of a class contained in the landscape.", call. = FALSE)
        }
    }

    if (length(class) > 1 & any(class %in% c("all", "global"))) {

        warning("'global' and 'all' can't be combined with any other class-argument.", call. = FALSE)
    }

    resolution_xy <- raster::res(landscape)
    landscape_padded_extent <- raster::extent(landscape) + (2 * resolution_xy)
    landscape_labeled_empty <- raster::raster(x = landscape_padded_extent,
                                              resolution = resolution_xy,
                                              crs = raster::crs(landscape))

    landscape_labeled <- get_patches(landscape, directions = directions)[[1]]

    for (i in seq_len(length(landscape_labeled) - 1)) {

        max(rcpp_get_unique_values(raster::as.matrix(landscape_labeled[[i]])))

        max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }

    boundary <- lapply(X = landscape_labeled, FUN = function(patches_class) {

        # if (!consider_boundary) {
        #
        #     landscape_padded <- pad_raster(patches_class,
        #                                    pad_raster_value = NA,
        #                                    pad_raster_cells = 1,
        #                                    global = FALSE,
        #                                    return_raster = FALSE)[[1]]
        #
        #     patches_class <- raster::setValues(landscape_labeled_empty, landscape_padded)
        # }

        class_edge <- get_boundaries(patches_class,
                                     consider_boundary = consider_boundary)[[1]]

        full_edge <- class_edge

        if (edge_depth > 1) {

            for (i in seq_len(edge_depth - 1)) {

                raster::values(class_edge)[raster::values(class_edge) == 1] <- NA

                class_edge <- get_boundaries(class_edge,
                                             consider_boundary)[[1]]

                full_edge[which(class_edge[] == 1)] <- 1
            }
        }

        raster::crop(full_edge, directions = 4, y = landscape)
    })

    # reset boundaries
    boundary <- lapply(X = seq_along(boundary),
                       FUN = function(i){
                           raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 1)] <- -999

                           raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)] <-
                               raster::values(landscape_labeled[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)]

                           return(boundary[[i]])
                       }
    )

    boundary_labeled_stack <- raster::as.data.frame(sum(raster::stack(boundary),
                                                        na.rm = TRUE),
                                                    xy = TRUE)
    names(boundary_labeled_stack) <- c("x", "y", "values")

    boundary_labeled_stack$class <-  raster::values(landscape)
    boundary_labeled_stack$core_label <- boundary_labeled_stack$values

    boundary_labeled_stack$values <-  ifelse(boundary_labeled_stack$values == -999, 0, 1)
    boundary_labeled_stack$core_label <- ifelse(boundary_labeled_stack$core_label == -999, as.numeric(NA), boundary_labeled_stack$core_label)

    if (!labels) {
        boundary_labeled_stack$core_label <- NA
    }

    if (any(class == "global")) {
        boundary_labeled_stack$class <- "global"
    }

    if (any(class != "global")) {

        if (any(!(class %in% "all"))) {
            class_index <- which(boundary_labeled_stack$class %in% class)
            boundary_labeled_stack <- boundary_labeled_stack[class_index, ]
        }
    }

    plot <- ggplot2::ggplot(boundary_labeled_stack, ggplot2::aes(x, y)) +
        ggplot2::coord_fixed() +
        ggplot2::geom_raster(ggplot2::aes(fill = factor(values))) +
        ggplot2::geom_text(ggplot2::aes_string(x = "x", y = "y", label = "core_label"),
                           colour = "white", na.rm = TRUE) +
        ggplot2::facet_wrap(~ class, nrow = nrow, ncol = ncol) +
        ggplot2::scale_fill_manual(values = c("grey60", "#E17C05"),
                                   na.value = "grey85") +
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
            panel.background = ggplot2::element_rect(fill = "grey85"),
            plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines"))

    return(plot)
}

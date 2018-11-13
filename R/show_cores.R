#' Show core area
#'
#' @description Show core area
#'
#' @param landscape Raster object
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param what How to show the core area: "global" (single map), "all" (every class as facet), or a vector with the specific classes one wants to show (every selected class as facet).
#' @param consider_boundary Logical if cells that only neighbour the landscape
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
#' show_cores(landscape, what = "global", labels = FALSE)
#'
#' # show the core area of every class as facet
#' show_cores(landscape, what = "all", labels = FALSE)
#'
#' # show only the core area of class 1 and 3
#' show_cores(landscape, what = c(2, 3), labels = TRUE)
#'
#' @aliases show_cores
#' @rdname show_cores
#'
#' @export
show_cores <- function(landscape,
                       directions,
                       what,
                       labels,
                       nrow,
                       ncol,
                       consider_boundary,
                       edge_depth) UseMethod("show_cores")


#' @name show_cores
#' @export
show_cores.RasterLayer <- function(landscape,
                                   directions = 8,
                                   what = "all",
                                   labels = TRUE,
                                   nrow = NULL,
                                   ncol = NULL,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    show_cores_intern(landscape,
                      directions = directions,
                      what = what,
                      labels = labels,
                      nrow = nrow,
                      ncol = ncol,
                      consider_boundary = consider_boundary,
                      edge_depth = edge_depth)
}

#' @name show_cores
#' @export
show_cores.RasterStack <- function(landscape,
                                   directions = 8,
                                   what = "all",
                                   labels = TRUE,
                                   nrow = NULL,
                                   ncol = NULL,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    lapply(X = raster::as.list(landscape),
           FUN = show_cores_intern,
           directions = directions,
           what = what,
           labels = labels,
           nrow = nrow,
           ncol = ncol,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth)
}

#' @name show_cores
#' @export
show_cores.RasterBrick <- function(landscape,
                                   directions = 8,
                                   what = "all",
                                   labels = TRUE,
                                   nrow = NULL,
                                   ncol = NULL,
                                   consider_boundary = FALSE,
                                   edge_depth = 1) {

    lapply(X = raster::as.list(landscape),
           FUN = show_cores_intern,
           directions = directions,
           what = what,
           labels = labels,
           nrow = nrow,
           ncol = ncol,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth)
}

#' @name show_cores
#' @export
show_cores.stars <- function(landscape,
                             directions = 8,
                             what = "all",
                             labels = TRUE,
                             nrow = NULL,
                             ncol = NULL,
                             consider_boundary = FALSE,
                             edge_depth = 1) {

    landscape <- methods::as(landscape, "Raster")

    lapply(X = landscape,
           FUN = show_cores_intern,
           directions = directions,
           what = what,
           labels = labels,
           nrow = nrow,
           ncol = ncol,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth)
}

#' @name show_cores
#' @export
show_cores.list <- function(landscape,
                            directions = 8,
                            what = "all",
                            labels = TRUE,
                            nrow = NULL,
                            ncol = NULL,
                            consider_boundary = FALSE,
                            edge_depth = 1) {

    lapply(X = landscape,
           FUN = show_cores_intern,
           directions = directions,
           what = what,
           labels = labels,
           nrow = nrow,
           ncol = ncol,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth)
}

show_cores_intern <- function(landscape, directions, what, labels, nrow, ncol,
                              consider_boundary, edge_depth ) {

    if(any(!(what %in% c("all", "global")))){
        if (!all(what %in% raster::unique(landscape))){
            stop("what must at least contain one value of a class contained in the landscape.", call. = FALSE)
        }
    }

    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)) {

        max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }

    boundary <- lapply(X = landscape_labeled, FUN = function(patches_class) {

        if(!isTRUE(consider_boundary)) {
            patches_class <- pad_raster(patches_class, pad_raster_value = NA,
                                        pad_raster_cells = 1,
                                        global = FALSE)
        }

        class_edge <- raster::boundaries(patches_class,
                                         directions = 4)

        full_edge <- class_edge

        if(edge_depth > 1){
            for(i in seq_len(edge_depth - 1)){

                raster::values(class_edge)[raster::values(class_edge) == 1] <- NA

                class_edge <- raster::boundaries(class_edge,
                                                 directions = 4)

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

    boundary_labeled_stack <- dplyr::mutate(boundary_labeled_stack,
                                            class = raster::values(landscape),
                                            core_label = values)

    boundary_labeled_stack <- dplyr::mutate(boundary_labeled_stack,
                                            values = dplyr::case_when(values == -999 ~ 0,
                                                                      values != -999 ~ 1),
                                            core_label = dplyr::case_when(core_label == -999 ~ as.numeric(NA),
                                                                          TRUE ~ core_label))

    if (!isTRUE(labels)){
        boundary_labeled_stack$core_label <- NA
    }

    if (any(what == "global")) {
        plot <- ggplot2::ggplot(boundary_labeled_stack) +
            ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = factor(values))) +
            ggplot2::geom_text(ggplot2::aes_string(x = "x", y = "y", label = "core_label"),
                               colour = "white") +
            ggplot2::coord_equal() +
            ggplot2::theme_void() +
            ggplot2::guides(fill = FALSE) +
            ggplot2::scale_fill_manual(values = c("grey60", "#E17C05"),
                                       na.value = "grey85") +
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
    }

    if (any(what != "global")) {

        if (any(!(what %in% "all"))){
            boundary_labeled_stack <- dplyr::filter(boundary_labeled_stack, class %in% what)
        }

        plot <- ggplot2::ggplot(boundary_labeled_stack, ggplot2::aes(x, y)) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes(fill = factor(values))) +
            ggplot2::geom_text(ggplot2::aes_string(x = "x", y = "y", label = "core_label"),
                               colour = "white") +
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
    }
    suppressWarnings(return(plot))
}

#' Show landscape metrics
#'
#' @description Show landscape metrics on patch level printed in their corresponding patch.
#'
#' @param landscape *Raster object
#' @param what Patch level what to plot
#' @param class How to show the labeled patches: "global" (single map), "all" (every class as facet),
#' or a vector with the specific classes one wants to show (every selected class as facet).
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param consider_boundary Logical if cells that only neighbour the landscape boundary should be considered as core
#' @param edge_depth Distance (in cells) a cell has the be away from the patch edge to be considered as core cell
#' @param labels Logical flag indicating whether to print or not to print patch labels.
#' @param label_lsm If true, the value of the landscape metric is used as label
#' @param nrow,ncol Number of rows and columns for the facet.
#'
#' @details The function plots all patches with a fill corresponding to the value of the chosen landscape metric on patch level.
#'
#' @return ggplot
#'
#' @examples
#' show_lsm(landscape, what = "lsm_p_area", directions = 4)
#' show_lsm(landscape, what = "lsm_p_shape", class = c(1, 2), label_lsm = TRUE)
#' show_lsm(landscape, what = "lsm_p_circle", class = 3, labels = TRUE)
#'
#' @aliases show_lsm
#' @rdname show_lsm
#'
#' @export
show_lsm <- function(landscape, what, class,
                     directions, consider_boundary, edge_depth,
                     labels, label_lsm,
                     nrow, ncol)  UseMethod("show_lsm")

#' @name show_lsm
#' @export
show_lsm.RasterLayer <- function(landscape,
                                 what,
                                 class = "global",
                                 directions = 8,
                                 consider_boundary = FALSE,
                                 edge_depth = 1,
                                 labels = TRUE,
                                 label_lsm = FALSE,
                                 nrow = NULL,
                                 ncol = NULL) {

    show_lsm_internal(landscape,
                      what = what,
                      class = class,
                      directions = directions,
                      consider_boundary = consider_boundary,
                      edge_depth = edge_depth,
                      labels = labels,
                      label_lsm = label_lsm,
                      nrow = nrow,
                      ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.RasterStack <- function(landscape,
                                 what,
                                 class = "global",
                                 directions = 8,
                                 consider_boundary = FALSE,
                                 edge_depth = 1,
                                 labels = TRUE,
                                 label_lsm = FALSE,
                                 nrow = NULL,
                                 ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_lsm_internal,
           what = what,
           class = class,
           directions = directions,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth,
           labels = labels,
           label_lsm = label_lsm,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.RasterBrick <- function(landscape,
                                 what,
                                 class = "global",
                                 directions = 8,
                                 consider_boundary = FALSE,
                                 edge_depth = 1,
                                 labels = TRUE,
                                 label_lsm = FALSE,
                                 nrow = NULL,
                                 ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_lsm_internal,
           what = what,
           class = class,
           directions = directions,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth,
           labels = labels,
           label_lsm = label_lsm,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.stars <- function(landscape,
                           what,
                           class = "global",
                           directions = 8,
                           consider_boundary = FALSE,
                           edge_depth = 1,
                           labels = TRUE,
                           label_lsm = FALSE,
                           nrow = NULL,
                           ncol = NULL) {

    landscape <- methods::as(landscape, "Raster")

    lapply(X = raster::as.list(landscape),
           FUN = show_lsm_internal,
           what = what,
           class = class,
           directions = directions,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth,
           labels = labels,
           label_lsm = label_lsm,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.list <- function(landscape,
                          what,
                          class = "global",
                          directions = 8,
                          consider_boundary = FALSE,
                          edge_depth = 1,
                          labels = TRUE,
                          label_lsm = FALSE,
                          nrow = NULL,
                          ncol = NULL) {

    lapply(X = landscape,
           FUN = show_lsm_internal,
           what = what,
           class = class,
           directions = directions,
           consider_boundary = consider_boundary,
           edge_depth = edge_depth,
           labels = labels,
           label_lsm = label_lsm,
           nrow = nrow,
           ncol = ncol)
}

show_lsm_internal <- function(landscape, what, class,
                              directions, consider_boundary, edge_depth,
                              labels, label_lsm,
                              nrow, ncol) {

    if (!what %in% list_lsm(level = "patch", simplify = TRUE) || length(what) > 1) {

        stop("Please provide one patch level metric only. To list available metrics, run list_lsm(level = 'patch').",
             call. = FALSE)
    }

    if (any(!(class %in% c("all", "global")))) {

        if (!any(class %in% raster::unique(landscape))) {

            stop("'class' must contain at least one value of a class existing in the landscape.",
                 call. = FALSE)
        }
    }

    if (length(class) > 1 & any(class %in% c("all", "global"))) {

        warning("'global' and 'all' can't be combined with any other class-argument.",
                call. = FALSE)
    }

    landscape_labeled <- get_patches(landscape, directions = directions)

    for (i in seq_len(length(landscape_labeled) - 1)) {

        max_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_id
    }

    lsm_fun <- match.fun(what)

    if (what %in% c("lsm_p_core", "lsm_p_ncore")) {

        fill_value <- lsm_fun(landscape,
                              directions = directions,
                              consider_boundary = consider_boundary,
                              edge_depth = edge_depth)
    }

    else {

        fill_value <- lsm_fun(landscape, directions = directions)
    }

    if (any(class == "global")) {

        patches_tibble <- raster::as.data.frame(sum(raster::stack(landscape_labeled),
                                                    na.rm = TRUE),
                                                xy = TRUE)

        names(patches_tibble) <- c("x", "y", "id")

        patches_tibble$id <- replace(patches_tibble$id,
                                     patches_tibble$id == 0,
                                     NA)

        patches_tibble <- merge(x = patches_tibble,
                                y = fill_value,
                                by = "id",
                                all.x = TRUE)

        patches_tibble$class.get_patches <- "global"

        if (!labels) {
            patches_tibble$label <- NA
        } else {
            if (label_lsm) {
                patches_tibble$label <- round(patches_tibble$value, 2)
            } else{
                patches_tibble$label <- patches_tibble$id
            }
        }
    }

    if (any(class != "global")) {

        patches_tibble <- lapply(X = seq_along(landscape_labeled), FUN = function(i){
            names(landscape_labeled[[i]]) <- "id"
            x <- raster::as.data.frame(landscape_labeled[[i]], xy = TRUE)
            x$class <- as.numeric(names(landscape_labeled[i]))
            return(x)}
        )

        patches_tibble <- do.call(rbind, patches_tibble)

        patches_tibble <- merge(x = patches_tibble,
                                y = fill_value,
                                by = "id",
                                all.x = TRUE,
                                suffixes = c(".get_patches", ".lsm"))

        if (any(!(class %in% "all"))) {

            class_index <- which(patches_tibble$class.get_patches %in% class)
            patches_tibble <- patches_tibble[class_index, ]
        }

        if (!labels) {

            patches_tibble$label <- NA
        }

        else {

            if (label_lsm) {

                patches_tibble$label <- round(patches_tibble$value, 2)
            }

            else{

                patches_tibble$label <- patches_tibble$id
            }
        }
    }

    plot <- ggplot2::ggplot(patches_tibble, ggplot2::aes(x, y)) +
        ggplot2::coord_fixed() +
        ggplot2::geom_raster(ggplot2::aes(fill = value)) +
        ggplot2::geom_text(ggplot2::aes(label = label),
                           colour = "black", size = 2, na.rm = TRUE)  +
        ggplot2::facet_wrap(~ class.get_patches,
                            nrow = nrow, ncol = ncol) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::labs(titel = NULL, x = NULL, y = NULL) +
        ggplot2::scale_fill_viridis_c(option = "E",
                                      name = what,
                                      na.value = "grey85") +
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

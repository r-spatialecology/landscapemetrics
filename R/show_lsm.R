#' Show landscape metrics
#'
#' @description Show patches
#'
#' @param landscape *Raster object
#' @param metric Patch level metric to plot
#' @param what How to show the labeled patches: "global" (single map), "all" (every class as facet),
#' or a vector with the specific classes one wants to show (every selected class as facet).
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param labels Logical flag indicating whether to print or not to print patch labels.

#'
#' @details The function plots all patches with a fill corresponding to the value of the chosen metric
#'
#' @return ggplot
#'
#' @examples
#' show_lsm(landscape, metric = "lsm_p_area")
#'
#' @aliases show_lsm
#' @rdname show_lsm
#'
#' @export
show_lsm <- function(landscape, metric, what, directions, labels, nrow, ncol)  UseMethod("show_lsm")

#' @name show_lsm
#' @export
show_lsm.RasterLayer <- function(landscape,
                                 metric,
                                 what = "global",
                                 directions = 8,
                                 labels = TRUE,
                                 nrow = NULL,
                                 ncol = NULL) {

    show_lsm_intern(landscape,
                    metric = metric,
                    what = what,
                    directions = directions,
                    labels = labels,
                    nrow = nrow,
                    ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.RasterStack <- function(landscape,
                                 metric,
                                 what = "global",
                                 directions = 8,
                                 labels = TRUE,
                                 nrow = NULL,
                                 ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_lsm_intern,
           metric = metric,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.RasterBrick <- function(landscape,
                                 metric,
                                 what = "global",
                                 directions = 8,
                                 labels = TRUE,
                                 nrow = NULL,
                                 ncol = NULL) {

    lapply(X = raster::as.list(landscape),
           FUN = show_lsm_intern,
           metric = metric,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.stars <- function(landscape,
                           metric,
                           what = "global",
                           directions = 8,
                           labels = TRUE,
                           nrow = NULL,
                           ncol = NULL) {

    landscape <- methods::as(landscape, "Raster")

    lapply(X = landscape,
           FUN = show_lsm_intern,
           metric = metric,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

#' @name show_lsm
#' @export
show_lsm.list <- function(landscape,
                          metric,
                          what = "global",
                          directions = 8,
                          labels = TRUE,
                          nrow = NULL,
                          ncol = NULL) {

    lapply(X = landscape,
           FUN = show_lsm_intern,
           metric = metric,
           what = what,
           directions = directions,
           labels = labels,
           nrow = nrow,
           ncol = ncol)
}

show_lsm_intern <- function(landscape, metric, what, directions, labels, nrow, ncol) {

    namespace_patch <- getNamespaceExports("landscapemetrics")
    namespace_patch <- namespace_patch[namespace_patch %in%
                                           grep("_p_", namespace_patch,
                                                value = TRUE)]
    namespace_patch <- namespace_patch[!grepl("\\.|calc", namespace_patch)]
    namespace_patch <- namespace_patch[!grepl("\\.|int", namespace_patch)]


    if(!exists("metric") || !metric %in% namespace_patch){
        stop("Please provide (only) one patch level metric", call. = FALSE)
    }

    if(any(!(what %in% c("all", "global")))){
        if (!any(what %in% raster::unique(landscape))){
            stop("'what must at least contain one value of a class contained in the landscape", call. = FALSE)
        }
    }

    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)){

        max_patch_id <- max(raster::values(landscape_labeled[[i]]), na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }

    lsm_fun <- match.fun(metric)

    fill_value <- lsm_fun(landscape, directions = directions)

    if (any(what == "global")) {

        landscape_labeled_stack <- raster::as.data.frame(sum(raster::stack(landscape_labeled),
                                                             na.rm = TRUE),
                                                         xy = TRUE)

        names(landscape_labeled_stack) <- c("x", "y", "patch_id")

        landscape_labeled_stack <- dplyr::mutate(landscape_labeled_stack,
                                                 patch_id = replace(patch_id,
                                                                    patch_id == 0, NA))

        landscape_labeled_stack <- dplyr::left_join(x = landscape_labeled_stack,
                                                    y = fill_value,
                                                    by = c("patch_id" = "id"))

        if (!isTRUE(labels)) {
            landscape_labeled_stack$patch_id <- NA
        }

        plot <- ggplot2::ggplot(landscape_labeled_stack) +
            ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = patch_id),
                               colour = "white") +
            ggplot2::coord_equal() +
            ggplot2::theme_void() +
            ggplot2::scale_fill_viridis_c(na.value = NA, name = metric) +
            ggplot2::theme(
                axis.title = ggplot2::element_blank(),
                axis.line = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank(),
                axis.title.x = ggplot2::element_blank(),
                axis.title.y = ggplot2::element_blank(),
                axis.ticks.length = ggplot2::unit(0, "lines"),
                panel.background = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.spacing = ggplot2::unit(0, "lines"),
                plot.background = ggplot2::element_blank(),
                plot.margin = ggplot2::unit(c(-1,-1,-1.5,-1.5), "lines")
            ) +
            ggplot2::labs(x = "global", y = "global")
    }

    if (any(what != "global")) {

        patches_tibble <- lapply(X = landscape_labeled, FUN = function(x){
            names(x) <- "patch_id"
            x <- raster::as.data.frame(x, xy = TRUE)
            return(x)}
        )

        patches_tibble <- dplyr::bind_rows(patches_tibble, .id = "class")

        patches_tibble <- dplyr::left_join(x = patches_tibble,
                                           y = fill_value,
                                           by = c("patch_id" = "id"),
                                           suffix = c(".get_patches", ".lsm"))

        if (any(!(what %in% "all"))){
            patches_tibble <- dplyr::filter(patches_tibble, class.get_patches %in% what)
        }

        if (!isTRUE(labels)) {
            patches_tibble$patch_id <- NA
        }

        plot <- ggplot2::ggplot(patches_tibble, ggplot2::aes(x, y)) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes(fill = value)) +
            ggplot2::geom_text(ggplot2::aes(label = patch_id),
                               colour = "white")  +
            ggplot2::scale_fill_viridis_c(na.value = NA, name = metric) +
            ggplot2::facet_wrap(~ class.get_patches,
                                nrow = nrow, ncol = ncol) +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
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

    suppressWarnings(return(plot))
}

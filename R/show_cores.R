#' Show core area
#'
#' @description Show core area
#'
#' @param landscape Raster object
#' @param directions The number of directions in which patches should be
#' connected: 4 (rook's case) or 8 (queen's case).
#' @param what How to show the core area: "global" (single map), "all" (every class as facet), or a vector with the specific classes one wants to show (every selected class as facet).
#' @param nrow,ncol Number of rows and columns for the facet.
#'
#' @details The functions plots the core area of patches labeled with the
#' corresponding patch id. The edges are the grey cells surrounding the patches and are always shown.
#'
#' @return ggplot
#'
#' @examples
#' # show "global" core area
#' show_cores(landscape, what = "global")
#'
#' # show the core area of every class as facet
#' show_cores(landscape, what = "all")
#'
#' # show only the core area of class 1 and 3
#' show_cores(landscape, what = c(1,3))
#'
#' @aliases show_cores
#' @rdname show_cores
#'
#' @export
show_cores <- function(landscape,
                       directions = 8,
                       what = "all",
                       nrow = NULL,
                       ncol = NULL) {

    if(any(!(what %in% c("all", "global")))){
        if (!all(what %in% raster::unique(landscape))){
            stop("what must at least contain one value of a class contained in the landscape.", call. = FALSE)
        }
    }


    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)){
        max_patch_id <- landscape_labeled[[i]] %>%
            raster::values() %>%
            max(na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }


    boundary <- purrr::map(landscape_labeled, raster::boundaries, directions = 4)

    # reset boundaries
    boundary <-  purrr::map(seq_along(boundary), function(i){
        raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 1)] <- -999
        return(boundary[[i]])
    })

    # label patches boundaries
    boundary <-  purrr::map(seq_along(boundary), function(i){
        raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)] <-
            raster::values(landscape_labeled[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)]
        return(boundary[[i]])
    })

    boundary_labeled_stack <- boundary %>%
        raster::stack() %>%
        sum(na.rm = TRUE) %>%
        raster::as.data.frame(xy = TRUE) %>%
        purrr::set_names("x", "y", "values") %>%
        mutate(class = raster::values(landscape))

    boundary_labeled_stack$values[boundary_labeled_stack$values == -999] <- NA

    if (any(what == "global")) {
        plot <- ggplot2::ggplot(boundary_labeled_stack) +
            ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = values)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values),
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
                ),
                na.value = "grey75") +
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

    if (any(what == "all")) {
        plot <- ggplot2::ggplot(boundary_labeled_stack, ggplot2::aes(x, y)) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes(fill = values)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = values),
                                colour = "white") +
            ggplot2::scale_fill_gradientn(
                colours = c("#E17C05"),
                na.value = "grey75") +
            ggplot2::facet_wrap(~class, nrow = 1, ncol = 3) +
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

    if (any(!(what %in% c("all", "global")))) {

        core_tibble <- purrr::map(boundary, function(x){
                tibble::as_tibble(expand.grid(x = seq(1, raster::ncol(x)),
                                                     y = seq(raster::nrow(x), 1))) %>%
                    dplyr::bind_cols(., z = raster::values(x))
            }) %>%
            purrr::set_names(seq_along(boundary)) %>%
            magrittr::extract(what) %>%
            dplyr::bind_rows(., .id = "id")

        core_tibble$patchlabel <- core_tibble$z
        core_tibble$patchlabel[core_tibble$patchlabel == -999] <- NA

        plot <- ggplot2::ggplot(core_tibble, ggplot2::aes(x, y)) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes(fill = z)) +
            ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = patchlabel),
                               colour = "white") +
            ggplot2::scale_fill_gradientn(
                colours = c("grey75","#E17C05"),
                na.value = NA) +
            ggplot2::facet_wrap(~id, nrow = 1, ncol = 3) +
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

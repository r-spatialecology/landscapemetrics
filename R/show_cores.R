show_cores_intern <- function(landscape,
                              directions = 8,
                              what = "all",
                              nrow = NULL,
                              ncol = NULL) {

    landscape_labeled <- get_patches(landscape, directions = directions)

    for(i in seq_len(length(landscape_labeled) - 1)){
        max_patch_id <- landscape_labeled[[i]] %>%
            raster::values() %>%
            max(na.rm = TRUE)

        landscape_labeled[[i + 1]] <- landscape_labeled[[i + 1]] + max_patch_id
    }


    boundary <- purrr::map(landscape_labeled, raster::boundaries, directions = 4)

    # reset boundaries
    boundary <-  purrr::map(seq_along(landscape_labeled), function(i){
        raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 1)] <- -999
        return(boundary[[i]])
    })

    # label patches boundaries
    boundary <-  purrr::map(seq_along(landscape_labeled), function(i){
        raster::values(boundary[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)] <- raster::values(landscape_labeled[[i]])[raster::values(!is.na(boundary[[i]])) & raster::values(boundary[[i]] == 0)]
        return(boundary[[i]])
    })

    boundary_labeled_stack <- boundary %>%
        raster::stack() %>%
        sum(na.rm = TRUE) %>%
        raster::as.data.frame(xy = TRUE) %>%
        purrr::set_names("x", "y", "values") %>%
        dplyr::mutate(values = replace(values, values == 0, NA))

    boundary_labeled_stack$values[boundary_labeled_stack$values == -10] <- NA

    if (what == "global") {
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
        return(plot)
    }

    if (what == "all") {


        patch_tibble <- tibble::enframe(boundary, "id", "maps") %>%
            dplyr::mutate(maps = purrr::map(.$maps, function(x){
                patch_tibble <- tibble::as_tibble(expand.grid(x = seq(1, raster::ncol(x)),
                                                     y = seq(raster::nrow(x), 1)))

                # Fill with raster values ----
                patch_tibble <- dplyr::bind_cols(patch_tibble, z = raster::values(x))
            })) %>%
            tidyr::unnest()

        patch_tibble$patchlabel <- patch_tibble$z
        patch_tibble$patchlabel[patch_tibble$patchlabel == -999] <- NA

        patch_tibble$z[patch_tibble$z != -999] <- 1

        plot <- ggplot2::ggplot(patch_tibble, ggplot2::aes_string("x", "y")) +
            ggplot2::coord_fixed() +
            ggplot2::geom_raster(ggplot2::aes_string(fill = "z")) +
            ggplot2::geom_text(ggplot2::aes_string(x = "x", y = "y", label = "patchlabel"),
                               colour = "white") +
            ggplot2::facet_wrap(~id, nrow = nrow, ncol = ncol) +
            ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0,max(patch_tibble$x))) +
            ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0,max(patch_tibble$y))) +
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
                plot.margin = ggplot2::unit(c(0, 0, 0, 0), "lines")
            )

        return(plot)
    }

}

show_patches(landscape)
show_cores_intern(landscape)

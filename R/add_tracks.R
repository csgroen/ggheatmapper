#' Add column tracks to a `ggheatmap`
#'
#' add_tracks() is a shortcut for building a new panel of `ggplot2::geom_tile()`s
#' with `track_columns` and adding it to the `ggheatmap`. It works if `track_columns`
#' are included in `ggData(gghm)`.
#'
#' @param gghm An ggplot object of class `ggheatmap`.
#' @param track_columns A vector with names of columns to be plotted.
#' @param track_colors A named list where names are the same as `track_columns`.
#' Can be either:
#' * NULL, in which case all colors will be chosen automatically.
#' * A valid palette used by `RColorBrewer`. See: [RColorBrewer::display.brewer.all()]
#' * A named vector of colors passed to [ggplot2::scale_fill_manual()].
#' @param track_prop A number between 0 and 1, representing the height
#' proportion between new tracks and the heatmap.
#' @param leg_ncol Number of columns in the track legends. Passed to
#' [ggplot2::guide_legend()].
#' @param fontsize Base fontsize for plot, which will be used by the theme.
#' Ultimately passed to [ggplot2::theme_minimal()] as `base_size`.
#' @param track_pos One of: 'bottom' or 'top'.
#' @param legend_action A string specifying how guides should be treated in the layout.
#' See: guides in [patchwork::plot_layout()].
#' @param show_legend If FALSE, no legend is added to the ggheatmap for the tracks
#'
#' @export
#' @importFrom dplyr select
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 guides guide_legend
#' @importFrom magrittr %>%
add_tracks <- function(gghm,
                       track_columns,
                       track_colors = list(),
                       track_prop = 0.3,
                       leg_ncol = 3,
                       fontsize = 11,
                       track_pos = "bottom",
                       legend_action = "collect",
                       show_legend = TRUE) {
    ppdf <- gghm$data %>%
        select(observations, {{ track_columns }})

    #-- Identify column types
    col_cls <- sapply(ppdf, class)[-1]

    #-- Get colors
    track_colors <- .get_trackColors(track_colors, track_columns, col_cls)

    #-- Track plots
    track_plots <- lapply(track_columns, .track_plot, ppdf, track_colors, col_cls,
                          fontsize, line_geom = gghm$gghm$line_geom, gghm$gghm$params[["show_rownames"]])

    track_plt_ptch <- wrap_plots(track_plots, ncol = 1, tag_level = 'new') &
        guides(fill = guide_legend(ncol = leg_ncol))

    if(!show_legend)
        track_plt_ptch <- track_plt_ptch & guides(fill = FALSE)

    #-- Align
    annot_hm <- align_to_hm(gghm, track_plt_ptch, pos = track_pos,
                            newplt_size_prop = track_prop, legend_action = legend_action)

    return(annot_hm)
}
#' Add column tracks as a matrix to a `ggheatmap`
#'
#' add_matrix_track() is a shortcut for building a new panel of `ggplot2::geom_tile()`s
#' with `track_columns` and adding it to the `ggheatmap` for a collection of
#' numeric columns. It works if `track_columns` are included in `ggData(gghm)`.
#'
#' @param gghm An ggplot object of class `ggheatmap`.
#' @param track_columns A vector with names of columns to be plotted.
#' @param track_colors A named list where names are the same as `track_columns`.
#' Can be either:
#' * A valid palette used by `RColorBrewer`. See: [RColorBrewer::display.brewer.all()]
#' * A named vector of colors passed to [ggplot2::scale_fill_manual()].
#' @param colors_title A title for the color legend
#' @param rows_title A title for the variables in the rows
#' @param track_prop A number between 0 and 1, representing the height
#' proportion between new tracks and the heatmap.
#' @param fontsize Base fontsize for plot, which will be used by the theme.
#' Ultimately passed to [ggplot2::theme_minimal()] as `base_size`.
#' @param track_pos One of: 'bottom' or 'top'.
#' @param legend_action A string specifying how guides should be treated in the layout.
#' See: guides in [patchwork::plot_layout()].
#' @param colorbar_dir one of "vertical" or "horizontal". See [ggplot2::guide_colorbar]
#'
#' @export
#' @importFrom dplyr ungroup select
#' @importFrom ggplot2 guides guide_colorbar
#' @importFrom magrittr %>%
add_matrix_track <- function(gghm,
                       track_columns,
                       track_colors = "Blues",
                       colors_title = "value",
                       rows_title = NULL,
                       track_prop = 0.3,
                       fontsize = 11,
                       track_pos = "bottom",
                       legend_action = "collect",
                       colorbar_dir = "vertical") {
    #-- Get data
    ppdf <- gghm$data %>%
        ungroup() %>%
        select(observations, {{ track_columns }})

    #-- Get plot
    mat_plt <- .matrix_track_plot(ppdf, track_columns, track_colors, colors_title, fontsize) +
        guides(fill = guide_colorbar(direction = colorbar_dir))

    if(!is.null(rows_title)) {
        mat_plt <- mat_plt +
            theme(axis.title.y = element_text(color = "black")) +
            labs(y = rows_title)
    }

    #-- Align
    annot_hm <- align_to_hm(gghm, mat_plt, pos = track_pos,
                            newplt_size_prop = track_prop,
                            legend_action = legend_action)

    return(annot_hm)

}

#-------------------------------------------------------------------------------
.get_trackColors <- function(track_colors, track_columns, col_cls) {
    pal_counterd <- 1
    pal_counterc <- 1
    for(tcol in track_columns) {
        if(is.null(track_colors[[tcol]])) {
            if (col_cls[tcol] %in% c("factor", "character", "Date")) {
                track_colors[[tcol]] <- .discrete_pals[pal_counterd]
                pal_counterd <- pal_counterd + 1
            } else {
                track_colors[[tcol]] <- .continuous_pals[pal_counterc]
                pal_counterc <- pal_counterc + 1
            }
        }
    }
    return(track_colors)
}

#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_tile labs scale_y_discrete scale_fill_brewer
#' scale_fill_distiller
.track_plot <- function(tcol, ppdf, track_colors, col_cls, fontsize, line_geom,
                        show_rownames) {
    #-- Plot
    tplot <- ppdf %>%
        pivot_longer(!! tcol) %>%
        ggplot(aes(observations, name, fill = value)) +
            geom_tile() +
        labs(fill = tcol) +
        .theme_track(fontsize)

    if(show_rownames) {
        tplot <- tplot + scale_y_discrete(expand = c(0,0))
    } else {
        tplot <- tplot + scale_y_discrete(expand = c(0,0), position = 'right')
    }

    #-- Adjust color
    tpal <- track_colors[[tcol]]
    if (length(tpal) == 1) {
        if(col_cls[tcol] %in% c("factor", "character", "Date")) {
            tplot <- tplot +
                scale_fill_brewer(palette = tpal)
        } else {
            tplot <- tplot +
                scale_fill_distiller(palette = tpal)
        }
    } else {
        if(col_cls[tcol] %in% c("factor", "character")) {
            tplot <- tplot +
                scale_fill_manual(values = tpal)
        } else {
            tplot <- tplot +
                scale_fill_gradientn(colors = tpal)
        }
    }
    tplot <- tplot + line_geom
    return(tplot)
}

#'@importFrom magrittr %>%
#'@importFrom ggplot2 ggplot aes geom_tile labs scale_fill_distiller
#'@importFrom tidyr pivot_longer
.matrix_track_plot <- function(ppdf, track_columns, track_colors, colors_title, fontsize) {
    ppdf_melt <- ppdf %>%
        pivot_longer(!observations) %>%
        mutate(name = factor(name, levels = rev(track_columns)))

    plt <- ggplot(ppdf_melt, aes(observations, name, fill = value)) +
        geom_tile() +
        labs(fill = colors_title) +
        .theme_track(base_size = fontsize)

    if(track_colors %in% .continuous_pals) {
        plt <- plt +
            scale_fill_distiller(palette = track_colors, direction = 1)
    } else {
        plt <- plt +
            scale_fill_gradientn(colors = track_colors)
    }
    return(plt)
}

.discrete_pals <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2",
                    "Set1", "Set2", "Set3")
.continuous_pals <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                      "RdYlBu", "RdYlGn", "Spectral", "Blues",
                      "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges",
                      "OrRd", "PuBu", "PuBuGn",
                      "PuRd", "Purples", "RdPu", "Reds", "YlGn",
                      "YlGnBu", "YlOrBr", "YlOrRd")

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
#'
#' @export
#' @import tidyverse patchwork
add_tracks <- function(gghm,
                       track_columns,
                       track_colors = list(),
                       track_prop = 0.3,
                       leg_ncol = 3,
                       fontsize = 11,
                       track_pos = "bottom",
                       legend_action = "collect") {
    ppdf <- gghm$data %>%
        select(observations, !!! track_columns)

    #-- Identify column types
    col_cls <- sapply(ppdf, class)[-1]

    #-- Get colors
    track_colors <- .get_trackColors(track_colors, track_columns)

    #-- Track plots
    track_plots <- lapply(track_columns, .track_plot, ppdf, track_colors, col_cls,
                          fontsize, line_geom = gghm$gghm$line_geom)

    track_plt_ptch <- wrap_plots(track_plots, ncol = 1, tag_level = 'new') &
        guides(fill = guide_legend(ncol = leg_ncol))

    #-- Align
    annot_hm <- align_to_hm(gghm, track_plt_ptch, pos = track_pos,
                            newplt_size_prop = track_prop, legend_action = legend_action)

    return(annot_hm)
}
#-------------------------------------------------------------------------------
.get_trackColors <- function(track_colors, track_columns) {
    pal_counter <- 1
    for(tcol in track_columns) {
        if(is.null(track_colors[[tcol]])) {
            track_colors[[tcol]] <- pal_collection[pal_counter]
            pal_counter <- pal_counter + 1
        }
    }
    return(track_colors)
}

#' @import tidyverse
.track_plot <- function(tcol, ppdf, track_colors, col_cls, fontsize, line_geom) {
    #-- Plot
    tplot <- ppdf %>%
        pivot_longer(!! tcol) %>%
        ggplot(aes(observations, name, fill = value)) +
            geom_tile() +
        scale_y_discrete(expand = c(0,0), position = "right") +
        labs(fill = tcol) +
        .theme_track(fontsize)
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

#-------------------------------------------------------------------------------

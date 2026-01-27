#' Annotate rows on gghm
#'
#' Adds row labels on the left or right side of the gghm. Especially useful
#' when there are too many `rowv`, but you'd still like to highlight certain
#' variables in the heatmap.
#'
#' @param gghm An ggplot object of class ggheatmap.
#' @param annot_labels A vector of labels that you'd like to annotate. Must have
#' been passed as `rowv` when creating the heatmap.
#' @param side Either 'left' or 'right' of the heatmap
#' @param annot_size Size of annotation text
#' @param add_margin Whether to add some margin to make extra space for the
#' annotations if they're overlapping other elements. Margin unit is mm.
#'
#' @export
#' @importFrom ggplot2 coord_cartesian theme margin
#' @importFrom ggrepel geom_text_repel
#' @importFrom tibble tibble
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
annotate_gghm <- function(gghm, annot_labels, side = "left",
                          annot_size = 2.5, add_margin = 0) {
    if(! "ggheatmap" %in% class(gghm)) {
        stop("`gghm` must be of class `ggheatmap`.")
    }
    #-- Get heatmap
    heatmap <- gghm$gghm$plots$hm
    #-- Get annotation labels
    data <- tibble(rows = annot_labels, labels = annot_labels) %>%
        filter(rows %in% levels(heatmap$data$rows))


    if(side == "left") {
        hm <- heatmap +
            coord_cartesian(clip = "off") +
            geom_text_repel(data = data,
                            aes(x = 0, y = rows, label = annot_labels),
                            hjust = 0.5,
                            na.rm = TRUE,
                            xlim = c(-Inf, 0),
                            size = annot_size,
                            segment.color = "grey60",
                            min.segment.length = 0.2) +
            theme(plot.margin = margin(0,0,0,add_margin, unit = "mm"))
    } else {
        ncols <- heatmap$data$observations %>% nlevels()
        hm <- heatmap +
            coord_cartesian(clip = "off") +
            geom_text_repel(data = data,
                            aes(x = ncols, y = rows, label = annot_labels),
                            hjust = 0.5,
                            na.rm = TRUE,
                            xlim = c(ncols+1, Inf),
                            size = annot_size,
                            segment.color = "grey60",
                            min.segment.length = 0.2) +
            theme(legend.margin = margin(0,0,0,add_margin, unit = "mm"))
    }

    new_gghm <- update_hmPlot(gghm, hm)
    return(new_gghm)
}

#' ggheatmap additional themes
#'
#' These themes are provided as helpers to build panels with `ggheatmap`. They
#' are all built on top of [ggplot2::theme_minimal()].
#'
#' `theme_sparse()` is the baseline simple and spartan theme to build plots that align
#' with columns of `ggheatmap`s. `theme_sparse2()` and `theme_sparse3()` are designed
#' for plots that will align to rows of the `ggheatmap`.
#' `theme_quant()` adds horizontal lines to `theme_sparse()` and can be a useful
#' upgrade for displaying numeric values in the y-axis.
#' `theme_scatter()` and `theme_clean()` are all-purpose themes for building ggplots.
#'
#' @param ... all arguments passed to `ggplot2::theme_minimal()`.
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text
#' margin
#' @export
theme_sparse <- function(...) {
    theme_minimal(...) +
        theme(panel.grid = element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_line(color = "black"),
              axis.text.y = element_text(color = "black"),
              plot.margin = margin(0,0,0,0))
}
#' @export
#' @rdname theme_sparse
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text margin
theme_sparse2 <- function(...) {
    theme_minimal(...) +
        theme(panel.grid = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_text(color = 'black'),
              axis.ticks.x = element_line(color = 'black'),
              axis.text.x = element_text(color = 'black'),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = margin(0,0,0,0))
}
#' @export
#' @rdname theme_sparse
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text margin
theme_sparse3 <- function(...) {
    theme_minimal(...) +
        theme(panel.grid = element_blank(),
              axis.title.y = element_text(color = 'black'),
              axis.title.x = element_text(color = 'black'),
              axis.ticks.x = element_line(color = 'black'),
              axis.text.x = element_text(color = 'black'),
              axis.text.y = element_text(color = 'black'),
              axis.ticks.y = element_line(color = 'black'),
              plot.margin = margin(0,0,0,0))
}
#' @export
#' @rdname theme_sparse
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text margin
theme_quant <- function(...) {
    theme_minimal(...) +
        theme(panel.grid = element_blank(),
              panel.grid.major.y = element_line(color = "grey30", linetype = "dotted"),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_line(color = "black"),
              axis.text.y = element_text(color = "black"),
              plot.margin = margin(0,0,0,0))
}
#' @importFrom ggplot2 theme_minimal theme element_blank element_line element_text margin
.theme_track <- function(...) {
    tm <- theme_minimal(...) +
        theme(panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              plot.margin = margin(0,0,0,0))

}
#' @importFrom ggplot2 theme_minimal theme element_blank element_line
#' element_text unit
.theme_heatmap <- function(pspace, base_size,  ...) {
    theme_minimal(base_size, ...) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
              axis.text.y = element_text(),
              plot.margin = margin(0,0,0,0),
              panel.spacing = unit(pspace, "pt"),
              legend.title = element_text(size = base_size),
              legend.text = element_text(size = base_size-1))
}

#' @export
#' @rdname theme_sparse
#' @importFrom ggplot2 theme_light theme element_blank element_line element_text
theme_scatter <- function(...) {
    theme_light(...) +
        theme(panel.border = element_rect(color = "black"),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              strip.background = element_rect(fill = "transparent", color = "black"),
              strip.text = element_text(color = "black"))
}
#' @export
#' @rdname theme_sparse
#' @importFrom ggplot2 theme_light theme element_blank element_line element_text element_rect
theme_clean <- function(...) {
    theme_light(...) +
        theme(panel.border = element_rect(color = "black"),
              panel.grid = element_blank(),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              strip.background = element_rect(fill = "transparent", color = "black"),
              strip.text = element_text(color = "black"))
}


#' @export
.pal_collection <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                    "Set2", "Set3", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                    "RdYlBu", "RdYlGn", "Spectral", "Blues", "BuGn", "BuPu", "GnBu",
                    "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd",
                    "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")

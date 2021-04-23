#' ggheatmap additional themes
#'
#' These themes are provided as helpers to build panels with `ggheatmap`. They
#' are all built on top of [ggplot2::theme_minimal()].
#'
#' `theme_sparse()` is the baseline simple and spartan theme to build plots that align
#' with columns of `ggheatmap`s. `theme_sparse2()` is designed for plots that will
#' align to rows of the `ggheatmap`. `theme_quant()` adds horizontal lines to
#' `theme_sparse()` and can be a useful upgrade for displaying numeric values in
#' the y-axis.
#'
#' @param ... all arguments passed to `ggplot2::theme_minimal()`.
#' @import tidyverse
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
#' @import tidyverse
.theme_track <- function(fontsize = 11) {
    theme_minimal(base_size = fontsize) +
        theme(panel.grid = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_blank(),
              axis.text.y = element_text(color = "black"),
              axis.ticks.y = element_line(color = "black"),
              plot.margin = margin(0,0,0,0))


}
#' @import tidyverse
.theme_heatmap <- function(...) {
    theme_minimal(...) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_line(color = "black"),
              axis.text = element_text(color = "black"),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              plot.margin = margin(0,0,0,0))
}

#' @export
.pal_collection <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1",
                    "Set2", "Set3", "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                    "RdYlBu", "RdYlGn", "Spectral", "Blues", "BuGn", "BuPu", "GnBu",
                    "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd",
                    "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"
)

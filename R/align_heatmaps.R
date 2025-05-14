#' Align plot to ggheatmap
#'
#' Align a ggheatmap object to another ggplot that shares either its columns or
#' rows to form complex panels. This function is called internally by add_tracks,
#' and also exported by ggheatmap.
#'
#' @param hm1 first heatmap
#' @param hm2 second heatmap
#' (if pos='top' or pos='bottom') or rows (if pos='left') with the heatmap.
#' Note: to create the gplot, make sure you use [gghmData()] to get the data, or
#' alternatively [get_rowLevels()] or [get_colLevels()].
#' @param pos 'vertical' or 'horizontal', by default the best option.
#' of the height (if pos='top' or pos='bottom') or width (if pos='left') that
#' the new plot will occupy in the panel.
#' @param legend_action A string specifying how guides should be treated in the layout.
#' See: guides in [patchwork::plot_layout()].
#' @param tag_level A string ('keep' or 'new') to indicate how auto-tagging should behave.
#' See [patchwork::plot_annotation].
#'
#' @import tidyverse patchwork
#' @importFrom rlist list.prepend list.append
#' @importFrom stringr str_split str_sub
#' @export
#' @return A joined panel object of class ggheatmap.
align_heatmaps <- function(hm1, hm2, pos = 'horizontal', legend_action = NULL, tag_level = 'new') {
    row1 <- hm1$gghm$row_levels
    row2 <- hm2$gghm$row_levels

    if (pos == "horizontal") {
        if (identical(row1, row2) == TRUE) {
            # Modifier le plot de hm2
            p <- hm2$gghm$plots[[4]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
            hm2$gghm$plots[[4]] <- p

            # Définir les nouvelles valeurs
            params <- hm1$gghm$params
            design <- hm1$gghm$design

            height <- hm1$gghm$params$heights
            plots <- c(hm1$gghm$plots, hm2$gghm$plots)

            prop <- 1 / length(plots) * 5

            widths <- c(params$widths * prop, hm2$gghm$params$widths * prop)

            # Créer un nouveau design
            to_add <- LETTERS[(length(hm1$gghm$plots) + 1):(length(plots))]
            the_end <- LETTERS[(length(plots) - 2):(length(plots) - 1)]
            to_add <- setdiff(to_add, the_end)

            lines <- str_split(hm1$gghm$design, "\n")[[1]]
            design_rows <- paste0(lines, "#", to_add)
            design_rows[length(design_rows)] <- paste0(lines[length(lines)], paste(the_end, collapse = ""))
            new_design <- paste(design_rows, collapse = "\n")

            # Générer le nouveau plot
            new_hm <- wrap_plots(plots, design = new_design,
                                 widths = widths,
                                 heights = height,
                                 guides = "collect")


            # Vérification pour s'assurer que le plot s'affiche correctement
            bug_check <- try(invisible(capture.output(print(new_hm))), silent = TRUE)
            if ("try-error" %in% class(bug_check)) {
                new_hm <- wrap_plots(new_hm, design = new_design,
                                     widths = widths,
                                     heights = hm1$gghm$heights)
            }
            #-- Update params

            class(new_hm) <- append(class(new_hm), "ggheatmap")
            new_hm$data <- hm1$data
            new_hm$gghm <- list(plots = plots,
                                params = params,
                                design = new_design,
                                row_levels = hm1$gghm$row_levels,
                                col_levels = hm1$gghm$col_levels,
                                line_geom = hm1$gghm$line_geom,
                                hclust = hm1$gghm$hclust)
        } else {
            stop("The row names must be the same.")
        }
    } else {
        new_hm <- align_to_hm(hm1, hm2)
    }
    return(new_hm)
}

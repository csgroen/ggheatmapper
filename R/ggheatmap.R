globalVariables(c("pal_collection", "observations", "rows", "name", "value"))
#' Plots a ggplot heatmap
#'
#' `ggheatmap()` is the main function of the `ggheatmap` package. It constructs
#' a tile-able plot using ggplot and patchwork, that can be added to and used in
#' constructing panels.
#'
#' @param table A table that can follow four formats:
#' * If `table` is a `data.frame` or `tbl` and `colv` and `rowv` are provided,
#' `colv` will be plotted into the columns (observations), while the variables
#' provided as `rowv` will be plotted into the rows (variables).
#' * If `table` is a `grouped_tbl` and `cluster_cols = TRUE`, a semi-supervised
#' clustering of the data will be performed instead only clustering within groups.
#' * If `table` is a `data.frame` or `tbl` and only `colv`, all other columns
#' will be used as `rowv`.
#' * If `table` is a `matrix` or `data.frame` and `colv` and `rowv` are `NULL`,
#' `ggheatmap` will plot it as-is (columns will be plotted as columns, rows as
#' rows of the heatmap). It is convenient, but less powerful.
#' @param colv Either NULL or the name of the column that contains the
#' observation ID variable for the data.
#' @param rowv Either NULL, a vector with names of columns to be plotted into the rows
#' of the heatmap, or a named list of vectors for a faceted plot (`show_dend_row` will then
#' be ignored).
#' @param hm_colors Can be either:
#' * A valid palette used by `RColorBrewer`. See: [RColorBrewer::display.brewer.all()]
#' * A vector of colors to be interpolated
#' @param hm_color_values Either NULL or a vector of values used for interpolation
#' between colors. Passed to `values` from [ggplot2::scale_fill_gradientn()]
#' @param hm_color_breaks Breaks for heatmap colors. Passed to `breaks` from
#' [ggplot2::scale_fill_gradientn()]
#' @param scale If TRUE, data will be scaled (z-score)
#' @param center If TRUE, data will be centered
#' @param cluster_rows If TRUE, rows will be clustered with [stats::hclust()]
#' @param cluster_cols If TRUE, columns will be clustered with [stats::hclust()]
#' @param dist_method Distance method passed to [stats::dist()]. Also supports
#' correlation distance (`1-cor()`), with `dist_method` passed to method from
#' `stats::cor`
#' @param clustering_method Clustering method is passed to method from
#'  `stats::hclust()`
#' @param raster If TRUE, [ggplot2::geom_raster()] will be used for the
#' heatmap tiles instead of [ggplot2::geom_tile()]. Will be recommended by
#' the function for large tables.
#' @param rows_title Label for row variables.
#' @param column_title Label for the column variable.
#' @param colors_title Labels for the color legend.
#' @param fontsize Base fontsize for plot, which will be used by the theme.
#' Ultimately passed to [ggplot2::theme_minimal()] as `base_size`.
#' @param show_rownames If TRUE, row names will be shown in the heatmap.
#' @param show_colnames If TRUE, col names will be shown in the heatmap.
#' @param show_dend_row If TRUE, the clustering dendrogram for the row variables
#' will be shown to the left of the heatmap
#' @param show_dend_col If TRUE, the clustering dendrogram for the column variable
#' will be shown on top of the heatmap.
#' @param dend_lwd Linewidth for the dendrogram drawing. Passed to
#' [ggtree::ggtree] as size.
#' @param dend_prop_row A value between 0 and 1. The proportion of the width
#' occupied by the dendrogram.
#' @param dend_prop_col A value between 0 and 1. The proportion of the height
#' occupied by the dendrogram.
#' @param group_track If `table` is a `grouped_tbl` and `group_track = TRUE`,
#' a track will be plot between the dendrogram and heatmap.
#' @param group_prop The proportion of the height of the heatmap that will be
#' used for the group track.
#' @param group_colors A named vector with colors for each level in the grouping
#' variable. If NULL, automatic colors will be used.
#' @param group_leg_ncol Number of columns in the groups legend. Passed to
#' [ggplot2::guide_legend()].
#'
#' @return A ggplot object with class `ggheatmap`.
#'
#' @importFrom stats dist hclust cor as.dist
#' @import tidyverse patchwork magrittr
#' @export
ggheatmap <- function(table,
                      colv = NULL,
                      rowv = NULL,
                      hm_colors = "RdYlBu",
                      hm_color_values = NULL,
                      hm_color_breaks = waiver(),
                      scale = FALSE,
                      center = FALSE,
                      cluster_rows = TRUE,
                      cluster_cols = TRUE,
                      dist_method = "euclidean",
                      clustering_method = "complete",
                      raster = FALSE,
                      rows_title = "",
                      column_title = "",
                      colors_title = "value",
                      fontsize = 11,
                      show_rownames = TRUE,
                      show_colnames = TRUE,
                      show_dend_row = TRUE,
                      show_dend_col = TRUE,
                      dend_lwd = 0.3,
                      dend_prop_row = 0.1,
                      dend_prop_col = 0.1,
                      group_track = TRUE,
                      group_prop = 0.1,
                      group_colors = NULL,
                      group_lines = FALSE,
                      group_line_color = "black",
                      group_lty = "solid",
                      group_lwd = 0.3,
                      group_leg_ncol = 3,
                      row_facetting_space = 3) {

    # Get variables if NULL
    if(is.null(colv)) {
        table <- t(table) %>%
            as.data.frame() %>%
            rownames_to_column("observations")
        colv <- "observations"
        show_dend_row = FALSE
    }
    if(is.null(rowv)) {
        rowv <- setdiff(colnames(table), colv)
    }
    # Check data
    .data_checks(table, colv, rowv)

    # Handle lists
    if(is.list(rowv)) {
        row_list <- rowv
        rowv <- unlist(rowv)
        names(rowv) <- NULL
    } else {
        row_list <- NULL
    }

    # Prepare data
    pptable <- .pp_data(table, colv, rowv, scale, center)

    # Raster warning
    if(nrow(pptable) > 10000 & !raster) {
        warning("Heatmap contains more than 10,000 tiles. Consider using `raster = TRUE` for a more manageable plot.")
    }

    # Cluster
    grouped <- is_grouped_df(table)
    facetted <- !is.null(row_list)
    cluster_res <- .cluster_data(table, pptable, grouped, colv, rowv,
                                 cluster_cols, facetted, cluster_rows,
                                 row_list, dist_method, clustering_method)
    pptable <- cluster_res$pptable
    cluster_obs <- cluster_res$cluster_obs

    # Plot heatmap
    gghm <- .plot_ggheatmap(pptable, hm_colors, hm_color_breaks,
                            rows_title, column_title, colors_title,
                            show_rownames, show_colnames, hm_color_values, raster,
                            fontsize, facetted, row_list, row_facetting_space) +
        plot_layout(tag_level = 'new')
    # Add lines
    line_geom <- .line_geom(table, grouped, group_lines, group_line_color,
                            group_lty, group_lwd)
    gghm <- gghm + line_geom
    # Get track
    if(grouped & group_track) {
        track_plot <- .plot_hm_track(table, pptable, group_colors, group_leg_ncol,
                                     fontsize, show_rownames) +
            plot_layout(tag_level = 'new') +
            line_geom
    } else {
        track_plot <- plot_spacer()
    }

    # Add dendro
    full_hm <- .heatmap_panel(gghm, cluster_obs, show_dend_row, show_dend_col,
                              dend_prop_col, dend_prop_row, dend_lwd,
                              track_plot, grouped, group_prop, cluster_rows, cluster_cols,
                              show_rownames, facetted) %>%
        suppressMessages()

    # Add data
    full_hm$data <- table %>%
        rename(observations = !! colv) %>%
        filter(observations %in% unique(pptable$observations)) %>%
        mutate(observations = factor(observations, levels = levels(pptable$observations)))
    full_hm$gghm$row_levels <- levels(pptable$rows)
    full_hm$gghm$col_levels <- levels(full_hm$data$observations)
    full_hm$gghm$line_geom <- line_geom

    return(full_hm)

}


#-------------------------------------------------------------------------------
# Patchwork
#' @import tidyverse
.heatmap_panel <- function(gghm, cluster_obs,
                           show_dend_row, show_dend_col,
                           dend_prop_col, dend_prop_row,
                           dend_lwd, track_plot, grouped, group_prop,
                           cluster_rows, cluster_cols,
                           show_rownames, facetted) {
    dend_row <- .plot_dendro(cluster_obs[["rows"]], type = "rows", dend_lwd) +
        labs(x = '') +
        plot_layout(tag_level = 'new') +
        theme(axis.title.x = element_text())
    dend_col <- .plot_dendro(cluster_obs[['cols']], type = "cols", dend_lwd) +
        plot_layout(tag_level = 'keep')
    h2 <- ifelse(grouped, group_prop, 0)

    if(show_dend_row & show_dend_col & cluster_rows & cluster_cols) {
        h1 <- dend_prop_col; h3 <- 1-h1-h2;
        w1 <- dend_prop_row; w2 <- 1-w1
        gghm <- gghm + scale_y_discrete(position = "right")
    } else if (show_dend_row & cluster_rows) {
        h1 <- 0; h3 <- 1-h2;
        w1 <- dend_prop_row; w2 <- 1-w1
    } else if (show_dend_col & cluster_cols) {
        h1 <- dend_prop_col; h3 <- 1-h1-h2
        w1 <- 0; w2 <- 1
    } else {
        h1 = 0; h3 = 1-h1-h2; w1 = 0; w2 = 1;
    }

    #-- Assemble plot
    full_hm <- (((plot_spacer() / plot_spacer() / dend_row +
                      plot_layout(heights = c(h1, h2, h3)))) |
        (dend_col / track_plot / gghm +
                      plot_layout(heights = c(h1, h2, h3)))) +
        plot_layout(widths = c(w1, w2),  tag_level = 'new') &
        theme(plot.margin = margin(0,0,0,0, unit = "pt"))
    full_hm <- wrap_plots(full_hm)

    #-- Get parameters
    class(full_hm) <- append(class(full_hm), "ggheatmap")
    full_hm$gghm <- list(plots = list('dend_col' = dend_col, 'group_track' = track_plot,
                                      'dend_row' = dend_row, 'hm' = gghm),
                         design = c("#A\n#B\nCD"))
    full_hm$gghm$params <- list(heights = c(h1,h2,h3),
                                widths = c(w1,w2),
                                hm_col = 2,
                                hm_row = 3,
                                show_rownames = show_rownames)

    return(full_hm)
}


#-------------------------------------------------------------------------------
# Plots
#' @import tidyverse
.plot_ggheatmap <- function(pptable, hm_colors, breaks,
                            rows_title, column_title, colors_title,
                            show_rownames, show_colnames, color_values, raster,
                            fontsize, facetted, row_list, row_facetting_space) {
    if(facetted) {
        # row_table <- stack(row_list) %>% as_tibble() %>% rename(rows = values, rgroup = ind)
        row_table <- tibble(rows = unlist(row_list),
                            rgroup = factor(rep(names(row_list), sapply(row_list, length)),
                                            levels = names(row_list)))
        pptable <- left_join(pptable, row_table, by = 'rows')

        if(show_rownames) {
            gghm <- ggplot(pptable) + facet_grid(rows = 'rgroup', scales = 'free_y', space = 'free_y')
        } else {
            gghm <- ggplot(pptable) + facet_grid(rows = 'rgroup', scales = 'free_y', space = 'free_y', switch = "y")
        }

    } else {
        gghm <- ggplot(pptable)
    }
    if(raster) {
        gghm <- gghm +
            geom_raster(aes(observations, rows, fill = value))
    } else {
        gghm <- gghm +
            geom_tile(aes(observations, rows, fill = value))
    }
    if (length(hm_colors) == 1 & hm_colors[1] %in% .pal_collection) {
        gghm <- gghm +
            scale_fill_distiller(palette = hm_colors, breaks = breaks, values = color_values)
    } else {
        gghm <- gghm +
            scale_fill_gradientn(colors = hm_colors, breaks = breaks, values = color_values)
    }
    gghm <- gghm +
        labs(x = column_title, y = rows_title, fill = colors_title) +
        .theme_heatmap(row_facetting_space, base_size = fontsize)

    if(!show_rownames) {
        gghm <- gghm +
            theme(axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
        }
    if(! show_colnames) {
        gghm <- gghm +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    }

    return(gghm)
}
.line_geom <- function(table, grouped, group_lines, group_line_color,
           group_lty, group_lwd) {
    grline_data <- table %>%
        summarize(n = n()) %>%
        mutate(gr_pos = cumsum(n) + 0.5) %>%
        slice(-n())
    line_geom <- geom_vline(aes(xintercept = gr_pos),
                            lty = group_lty, color = group_line_color,
                            size = group_lwd,
                            data = grline_data)
}


#' @importFrom ggtree ggtree rotate
#' @import tidyverse
.plot_dendro <- function(cluster_obj, type = "cols", dend_lwd) {
    if(is.null(cluster_obj))
        return(plot_spacer())

    if(type == "cols") {
        dend_plot <-  ggtree(cluster_obj, size = dend_lwd)
        root <- which(dend_plot[[1]]$parent == dend_plot[[1]]$node)
        dendro <- dend_plot +
            rotate(node = root) +
            coord_flip() +
            scale_x_reverse()
        dend_plot <- dendro[[2]]

    } else if (type == "rows") {
        dend_plot <- ggtree(cluster_obj, size = dend_lwd)
    }
    dend_plot +
        theme(plot.margin = margin(0,0,0,0))
}
#' @import tidyverse
.plot_hm_track <- function(table, pptable, group_colors, leg_ncol, fontsize,
                           show_rownames) {
    track_plot <- pptable %>%
        select(observations, group_var = group_vars(table)) %>%
        distinct() %>%
        mutate(group = group_vars(table)) %>%
        ggplot(aes(observations, group, fill = group_var)) +
        geom_raster() +
        labs(fill = group_vars(table)) +
        guides(fill = guide_legend(ncol = leg_ncol)) +
        .theme_track(fontsize)

    if(show_rownames) {
        track_plot <- track_plot + scale_y_discrete(expand = c(0,0))
    } else {
        track_plot <- track_plot + scale_y_discrete(expand = c(0,0), position = 'right')
    }

    if(!is.null(group_colors)) {
        track_plot <- track_plot +
            scale_fill_manual(values = group_colors)
    }
    return(track_plot)
}

#-------------------------------------------------------------------------------
# Clustering
#' @import tidyverse
.cluster_data <- function(table, pptable, grouped, colv,
                          rowv, cluster_cols, facetted,
                          cluster_rows, row_list,
                          dist_method, clustering_method) {
    cluster_obs <- list(rows = NULL, cols = NULL)
    pp_mat <- .pp_mat(pptable)

    #------- observations
    if(cluster_cols) {
        if(grouped) {
            gr_var <- group_vars(table)
            gr_table <- pptable %>% select(observations, group = !! gr_var) %>% distinct()
            groups <- split(gr_table$observations, gr_table$group)
            hcc_semi <- hclust_semisupervised(pp_mat, groups,
                                              dist_method = dist_method,
                                              hclust_method = clustering_method,
                                              cor_use = "pair")
            pptable <- pptable %>%
                mutate(observations = factor(observations, levels = rownames(hcc_semi$data)[hcc_semi$hclust$order])) %>%
                arrange(observations)
            cluster_obs[["cols"]] <- hcc_semi$hclust
        } else {
            if (class(cluster_cols) == "hclust") {
                hcc <- cluster_cols
            } else {
                hcc <- .hclust_data(pp_mat, dist_method, clustering_method)
            }
            pptable <- pptable %>%
                mutate(observations = factor(observations, levels = levels(observations)[hcc$order])) %>%
                arrange(observations)
            cluster_obs[["cols"]] <- hcc
        }
    }
    #------ Rows
    if (cluster_rows) {
        if(facetted) {
            hcc_semi <- hclust_semisupervised(t(pp_mat), row_list,
                                              dist_method = dist_method,
                                              hclust_method = clustering_method,
                                              cor_use = "pair")
            pptable <- pptable %>%
                mutate(rows = factor(rows, levels = rownames(hcc_semi$data)[hcc_semi$hclust$order])) %>%
                arrange(rows)

            cluster_obs[["rows"]] <- hcc_semi$hclust
        } else {
            if (class(cluster_rows) == "hclust") {
                hcr <- cluster_rows
            } else {
                hcr <- .hclust_data(t(pp_mat), dist_method, clustering_method)
            }
            pptable <- pptable %>%
                mutate(rows = factor(rows, levels = colnames(pp_mat)[hcr$order]))
            cluster_obs[["rows"]] <- hcr
        }
    }  else {
        pptable <- mutate(pptable, rows = factor(rows, levels = rowv))
    }
    return(list(pptable = pptable, cluster_obs = cluster_obs))
}

.hclust_data <- function(pp_mat, dist_method, clustering_method) {
    if(dist_method %in% c("pearson", "spearman", "kendall")) {
        d <- as.dist(1 - cor(t(pp_mat), method = dist_method, use = "pair"))
    } else {
        d <- dist(pp_mat, method = dist_method)
    }
    return(hclust(d, method = clustering_method))

}

#-------------------------------------------------------------------------------
# Data reshaping
#-- Reshape data for hclust
#' @import tidyverse
.pp_mat <- function(pptable) {
    ppmat <- pptable %>%
        select(rows, observations, value) %>%
        pivot_wider(id_cols = observations, names_from = rows, values_from = value) %>%
        arrange(observations) %>%
        as.data.frame() %>%
        column_to_rownames("observations")
    return(ppmat)
}

#-- Melt data for geom_tile
#' @import tidyverse
.pp_data <- function(table, colv, rowv, scale, center) {
    #-- Reshape if necessary
    table <- rename(table, observations = !! colv)
    if(!is.factor(table$observations)) {
        table <- mutate(table, observations = factor(observations))
    }
    table <- pivot_longer(table, !! rowv, names_to = "rows")

    #-- Scale if necessary
    if(scale | center) {
        table <- table %>%
            group_by(rows) %>%
            mutate(value = as.vector(scale(value, center = center, scale = scale))) %>%
            ungroup()
    }
    #-- Remove if all NA
    nvar <- length(unique(table$rows))
    na_rep <- table %>%
        group_by(observations) %>%
        summarize(nas = sum(is.na(value))) %>%
        filter(nas == nvar) %>%
        pull(observations)
    if(length(na_rep) > 0) {
        warning("Removing observations: ", paste0(na_rep, collapse = ", "))
        table <- table %>%
            filter(! observations %in% na_rep) %>%
            mutate(observations = fct_drop(observations, as.character(na_rep)))
    }

    return(table)
}
#-------------------------------------------------------------------------------
# Checks
#' @import tidyverse
.data_checks <- function(table, colv, rowv) {
    if(! colv %in% colnames(table)) {
        stop('`colv` must be a column in `table`')
    } else {
        if (anyDuplicated(pull(table, !! colv))) {
            stop('All items in `colv` must be unique.')
        }
    }
    if (is.list(rowv)) {
        if(! all(unlist(rowv) %in% colnames(table))) {
            stop('All elements of `rowv` must in columns in `table`.')
        }
        if(is.null(names(rowv))) {
            stop('If `rowv` is a list, elements must be a named')
        }
    } else if (! all(rowv %in% colnames(table))) {
            stop('All `rowv` must in columns in `table`.')
    } else {
        num_check <- table[,rowv] %>% apply(2, is.numeric) %>% all()
        if(!num_check) {
            stop('All `rowv` must be numeric variables.')
        }
    }

    invisible(TRUE)
}








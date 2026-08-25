# Plots a ggplot heatmap

`ggheatmap()` is the main function of the `ggheatmap` package. It
constructs a tile-able plot using ggplot and patchwork, that can be
added to and used in constructing panels.

## Usage

``` r
ggheatmap(
  table,
  colv = NULL,
  rowv = NULL,
  hm_colors = "RdYlBu",
  hm_color_values = NULL,
  hm_color_breaks = waiver(),
  hm_color_limits = NULL,
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
  group_label = FALSE,
  group_track_topslack = 0,
  group_label_angle = 0,
  group_label_size = 3.5,
  group_label_position = "center",
  group_prop = 0.1,
  group_colors = NULL,
  group_lines = FALSE,
  group_line_color = "black",
  group_lty = "solid",
  group_lwd = 0.3,
  group_leg_ncol = 3,
  row_facetting_space = 3,
  colorbar_dir = "vertical"
)
```

## Arguments

- table:

  A table that can follow four formats:

  - If `table` is a `data.frame` or `tbl` and `colv` and `rowv` are
    provided, `colv` will be plotted into the columns (observations),
    while the variables provided as `rowv` will be plotted into the rows
    (variables).

  - If `table` is a `grouped_tbl` and `cluster_cols = TRUE`, a
    semi-supervised clustering of the data will be performed instead
    only clustering within groups.

  - If `table` is a `data.frame` or `tbl` and only `colv`, all other
    columns will be used as `rowv`.

  - If `table` is a `matrix` or `data.frame` and `colv` and `rowv` are
    `NULL`, `ggheatmap` will plot it as-is (columns will be plotted as
    columns, rows as rows of the heatmap). It is convenient, but less
    powerful.

- colv:

  Either NULL or the name of the column that contains the observation ID
  variable for the data.

- rowv:

  Either NULL, a vector with names of columns to be plotted into the
  rows of the heatmap, or a named list of vectors for a faceted plot
  (`show_dend_row` will then be ignored).

- hm_colors:

  Can be either:

  - A valid palette used by `RColorBrewer`. See:
    [`RColorBrewer::display.brewer.all()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

  - A vector of colors to be interpolated

- hm_color_values:

  Either NULL or a vector of values used for interpolation between
  colors. Passed to `values` from
  [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)

- hm_color_breaks:

  Breaks for heatmap colors. Passed to `breaks` from
  [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)

- hm_color_limits:

  A vector with length = 2 for limits of the heatmap color scale. Passed
  to `limits` from
  [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

- scale:

  If TRUE, data will be scaled (z-score)

- center:

  If TRUE, data will be centered

- cluster_rows:

  If TRUE, rows will be clustered with
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)

- cluster_cols:

  If TRUE, columns will be clustered with
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)

- dist_method:

  Distance method passed to
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html). Also supports
  correlation distance (`1-cor()`), with `dist_method` passed to method
  from [`stats::cor`](https://rdrr.io/r/stats/cor.html)

- clustering_method:

  Clustering method is passed to method from
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)

- raster:

  If TRUE,
  [`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)
  will be used for the heatmap tiles instead of
  [`ggplot2::geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html).
  Will be recommended by the function for large tables.

- rows_title:

  A title for row variables.

- column_title:

  A title for the column variable.

- colors_title:

  A title for the color legend.

- fontsize:

  Base fontsize for plot, which will be used by the theme. Ultimately
  passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  as `base_size`.

- show_rownames:

  If TRUE, row names will be shown in the heatmap.

- show_colnames:

  If TRUE, column names will be shown in the heatmap.

- show_dend_row:

  If TRUE, the clustering dendrogram for the row variables will be shown
  to the left of the heatmap

- show_dend_col:

  If TRUE, the clustering dendrogram for the column variable will be
  shown on top of the heatmap.

- dend_lwd:

  Linewidth for the dendrogram drawing. Passed to
  [ggtree::ggtree](https://rdrr.io/pkg/ggtree/man/ggtree.html) as size.

- dend_prop_row:

  A value between 0 and 1. The proportion of the width occupied by the
  dendrogram.

- dend_prop_col:

  A value between 0 and 1. The proportion of the height occupied by the
  dendrogram.

- group_track:

  If `table` is a `grouped_tbl` and `group_track = TRUE`, a track will
  be plot between the dendrogram and heatmap.

- group_label:

  a logical indicating whether to label the groups directly. If TRUE,
  `show_dend_col` will be set to FALSE.

- group_track_topslack:

  a numeric, indicating space to add at the top of the group track.
  Useful for fitting names

- group_label_angle:

  an angle for the group labels

- group_label_size:

  a numeric value to set the size of the group labels

- group_label_position:

  one of: "left", "right" or "center", indicating where to write the
  group label if \`group_label = TRUE

- group_prop:

  The proportion of the height of the heatmap that will be used for the
  group track.

- group_colors:

  A named vector with colors for each level in the grouping variable. If
  NULL, automatic colors will be used.

- group_lines:

  If TRUE, vertical lines will separate supervised clustering groups

- group_line_color:

  Color of the vertical line

- group_lty:

  Group line type. See
  [graphics::par](https://rdrr.io/r/graphics/par.html)

- group_lwd:

  Group line width. See
  [graphics::par](https://rdrr.io/r/graphics/par.html)

- group_leg_ncol:

  Number of columns in the groups legend. Passed to
  [`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- row_facetting_space:

  space in pts between row facets, if they exist

- colorbar_dir:

  Places the `colorbar` either horizontal or vertically. See:
  [ggplot2::guide_colorbar](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)

## Value

A ggplot object with class `ggheatmap`.

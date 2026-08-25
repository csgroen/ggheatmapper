# Add column tracks as a matrix to a `ggheatmap`

add_matrix_track() is a shortcut for building a new panel of
[`ggplot2::geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)s
with `track_columns` and adding it to the `ggheatmap` for a collection
of numeric columns. It works if `track_columns` are included in
`ggData(gghm)`.

## Usage

``` r
add_matrix_track(
  gghm,
  track_columns,
  track_colors = "Blues",
  pal_dir = 1,
  colors_title = "value",
  color_limits = NULL,
  rows_title = NULL,
  show_colnames = FALSE,
  track_prop = 0.3,
  fontsize = 11,
  track_pos = "bottom",
  legend_action = "collect"
)
```

## Arguments

- gghm:

  An ggplot object of class `ggheatmap`.

- track_columns:

  A vector with names of columns to be plotted.

- track_colors:

  A named list where names are the same as `track_columns`. Can be
  either:

  - A valid palette used by `RColorBrewer`. See:
    [`RColorBrewer::display.brewer.all()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

  - A vector of colors passed to
    [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

- pal_dir:

  palette direction, used if `track_colors` is a palette name. See:
  [`ggplot2::scale_color_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)

- colors_title:

  A title for the color legend

- color_limits:

  NULL or a vector of two values, lower and upper limits for the colors.
  See:
  [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

- rows_title:

  A title for the variables in the rows

- show_colnames:

  If TRUE, column names will be shown in the track plot.

- track_prop:

  A number between 0 and 1, representing the height proportion between
  new tracks and the heatmap.

- fontsize:

  Base fontsize for plot, which will be used by the theme. Ultimately
  passed to
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  as `base_size`.

- track_pos:

  One of: 'bottom' or 'top'.

- legend_action:

  A string specifying how guides should be treated in the layout. See:
  guides in
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html).

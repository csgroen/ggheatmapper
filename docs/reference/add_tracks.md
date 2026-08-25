# Add column tracks to a `ggheatmap`

add_tracks() is a shortcut for building a new panel of
[`ggplot2::geom_tile()`](https://ggplot2.tidyverse.org/reference/geom_tile.html)s
with `track_columns` and adding it to the `ggheatmap`. It works if
`track_columns` are included in `ggData(gghm)`.

## Usage

``` r
add_tracks(
  gghm,
  track_columns,
  track_colors = list(),
  pal_dir = 1,
  track_prop = 0.3,
  leg_ncol = 3,
  fontsize = 11,
  track_pos = "bottom",
  legend_action = "collect",
  show_legend = TRUE
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

  - NULL, in which case all colors will be chosen automatically.

  - A valid palette used by `RColorBrewer`. See:
    [`RColorBrewer::display.brewer.all()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

  - A named vector of colors passed to
    [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

- pal_dir:

  palette direction, used if `track_colors` is a palette name. See:
  [`ggplot2::scale_color_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)

- track_prop:

  A number between 0 and 1, representing the height proportion between
  new tracks and the heatmap.

- leg_ncol:

  Number of columns in the track legends. Passed to
  [`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

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

- show_legend:

  If FALSE, no legend is added to the ggheatmap for the tracks

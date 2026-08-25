# Align plot to ggheatmap

Align a ggheatmap object to another ggplot that shares either its
columns or rows to form complex panels. This function is called
internally by add_tracks, and also exported by ggheatmap.

## Usage

``` r
align_heatmaps(
  hm1,
  hm2,
  pos = "horizontal",
  legend_action = NULL,
  tag_level = "new"
)
```

## Arguments

- hm1:

  first heatmap

- hm2:

  second heatmap (if pos='top' or pos='bottom') or rows (if pos='left')
  with the heatmap. Note: to create the gplot, make sure you use
  [`gghmData()`](get_data.md) to get the data, or alternatively
  [`get_rowLevels()`](get_data.md) or [`get_colLevels()`](get_data.md).

- pos:

  'vertical' or 'horizontal', by default the best option. of the height
  (if pos='top' or pos='bottom') or width (if pos='left') that the new
  plot will occupy in the panel.

- legend_action:

  A string specifying how guides should be treated in the layout. See:
  guides in
  [`patchwork::plot_layout()`](https://patchwork.data-imaginist.com/reference/plot_layout.html).

- tag_level:

  A string ('keep' or 'new') to indicate how auto-tagging should behave.
  See
  [patchwork::plot_annotation](https://patchwork.data-imaginist.com/reference/plot_annotation.html).

## Value

A joined panel object of class ggheatmap.

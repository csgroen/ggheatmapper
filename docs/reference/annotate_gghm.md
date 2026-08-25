# Annotate rows on gghm

Adds row labels on the left or right side of the gghm. Especially useful
when there are too many `rowv`, but you'd still like to highlight
certain variables in the heatmap.

## Usage

``` r
annotate_gghm(
  gghm,
  annot_labels,
  side = "left",
  annot_size = 2.5,
  add_margin = 0
)
```

## Arguments

- gghm:

  An ggplot object of class ggheatmap.

- annot_labels:

  A vector of labels that you'd like to annotate. Must have been passed
  as `rowv` when creating the heatmap.

- side:

  Either 'left' or 'right' of the heatmap

- annot_size:

  Size of annotation text

- add_margin:

  Whether to add some margin to make extra space for the annotations if
  they're overlapping other elements. Margin unit is mm.
